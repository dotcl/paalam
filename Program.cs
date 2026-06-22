// paalam — Avalonia image viewer (dotcl embedded host, cross-platform)
//
// C# only does boot and assembly pre-loading. UI/logic lives in src/main.lisp.
// Both dotcl/Avalonia are via NuGet only (no dependency on the dotcl repo; #281).

using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;
using Avalonia.Input;
using Avalonia.Platform.Storage;
using MetadataExtractor;
using MetadataExtractor.Formats.Exif;
using DotCL;

namespace Paalam;

// (The old Paalam.Imaging.BitmapFromStream shim was removed. dotcl-internal #294
//  was fixed in DotCL.Runtime 0.1.12, so (dotnet:new Bitmap stream) from Lisp can
//  now correctly select Bitmap(Stream).)

// Rasterize a PDF page by page with PDFium (PDFtoImage) into an Avalonia Bitmap.
// PDFium/SkiaSharp interop is confined to the C# side (Lisp only calls PageCount/RenderPage).
public sealed class PdfDoc
{
    private readonly byte[] _bytes;
    public PdfDoc(string path) => _bytes = System.IO.File.ReadAllBytes(path);
    // Raw PDF bytes, for Lisp's pdfium FFI (text/metadata/outline; see src/pdf-ffi.lisp).
    public byte[] Bytes => _bytes;
    public int PageCount => PDFtoImage.Conversion.GetPageCount(_bytes);
    // dpi is the rasterization resolution (Lisp's *pdf-dpi*). At 4K, 150 gets scaled
    // up and looks coarse, so the default is 300, configurable. Higher is finer but
    // uses more memory/time.
    public Avalonia.Media.Imaging.Bitmap RenderPage(int index, int dpi)
    {
        var opts = new PDFtoImage.RenderOptions(Dpi: dpi,
                       BackgroundColor: SkiaSharp.SKColors.White);
        using var sk = PDFtoImage.Conversion.ToImage(_bytes, index, null, opts);
        using var data = sk.Encode(SkiaSharp.SKEncodedImageFormat.Png, 90);
        using var ms = new System.IO.MemoryStream(data.ToArray());
        return new Avalonia.Media.Imaging.Bitmap(ms);
    }

    // (Text search / match rects / outline / metadata moved to Lisp pdfium FFI: src/pdf-ffi.lisp.)

    // For material export: return a page as a PNG byte array.
    public byte[] RenderPagePng(int index, int dpi)
    {
        var opts = new PDFtoImage.RenderOptions(Dpi: dpi,
                       BackgroundColor: SkiaSharp.SKColors.White);
        using var sk = PDFtoImage.Conversion.ToImage(_bytes, index, null, opts);
        using var data = sk.Encode(SkiaSharp.SKEncodedImageFormat.Png, 90);
        return data.ToArray();
    }
}

// EXIF / image metadata extraction (#36 Phase 2). MetadataExtractor (pure managed,
// cross-platform) reads from the page's raw bytes. Kept on the C# side so Lisp gets a
// simple "label<TAB>value" line string (same pattern as PdfText.Outline).
public static class Exif
{
    public static string Summary(byte[] bytes)
    {
        if (bytes == null || bytes.Length == 0) return "";
        try
        {
            using var ms = new MemoryStream(bytes);
            var dirs = ImageMetadataReader.ReadMetadata(ms);
            var ifd0 = dirs.OfType<ExifIfd0Directory>().FirstOrDefault();
            var sub  = dirs.OfType<ExifSubIfdDirectory>().FirstOrDefault();
            var gps  = dirs.OfType<GpsDirectory>().FirstOrDefault();
            var sb = new System.Text.StringBuilder();
            void Add(string label, string? value)
            {
                if (!string.IsNullOrWhiteSpace(value))
                    sb.Append(label).Append('\t')
                      .Append(value.Replace('\t', ' ').Replace('\n', ' ').Replace('\r', ' '))
                      .Append('\n');
            }
            void AddTag(MetadataExtractor.Directory? dir, string label, int tag)
            {
                if (dir != null) Add(label, dir.GetDescription(tag));
            }
            AddTag(sub,  "Taken",        ExifDirectoryBase.TagDateTimeOriginal);
            AddTag(ifd0, "Camera Make",  ExifDirectoryBase.TagMake);
            AddTag(ifd0, "Camera Model", ExifDirectoryBase.TagModel);
            AddTag(sub,  "Lens",         ExifDirectoryBase.TagLensModel);
            AddTag(sub,  "Aperture",     ExifDirectoryBase.TagFNumber);
            AddTag(sub,  "Exposure",     ExifDirectoryBase.TagExposureTime);
            AddTag(sub,  "ISO",          ExifDirectoryBase.TagIsoEquivalent);
            AddTag(sub,  "Focal Length", ExifDirectoryBase.TagFocalLength);
            AddTag(ifd0, "Orientation",  ExifDirectoryBase.TagOrientation);
            if (gps != null && gps.GetGeoLocation() is { } loc)
            {
                var ic = System.Globalization.CultureInfo.InvariantCulture;
                // decimal "lat, lon" — readable and directly usable in a map URL (#36 Phase 3).
                Add("GPS", loc.Latitude.ToString(ic) + ", " + loc.Longitude.ToString(ic));
            }
            return sb.ToString();
        }
        catch { return ""; }
    }
}

// (PDF text search / match rects / outline / metadata were the Paalam.PdfText [DllImport]
//  P/Invoke shim; now done directly in Lisp via dotnet:ffi — see src/pdf-ffi.lisp.)

// Save dialog (choosing where to export materials). async runs on the UI thread and,
// once confirmed, calls onPicked(path) (without freezing the UI).
public static class DialogHelper
{
    public static async void PickSaveZip(Avalonia.Controls.Window win, string suggestedName,
                                         Action<string> onPicked)
    {
        try
        {
            var file = await win.StorageProvider.SaveFilePickerAsync(new FilePickerSaveOptions
            {
                SuggestedFileName = suggestedName,
                DefaultExtension = "zip",
                FileTypeChoices = new[]
                {
                    new FilePickerFileType("Zip archive") { Patterns = new[] { "*.zip" } }
                }
            });
            if (file != null)
            {
                var p = file.TryGetLocalPath();
                if (!string.IsNullOrEmpty(p)) onPicked(p!);
            }
        }
        catch { /* ignore cancellation etc. */ }
    }

    public static async void PickOpenBook(Avalonia.Controls.Window win, Action<string> onPicked)
    {
        try
        {
            var files = await win.StorageProvider.OpenFilePickerAsync(new FilePickerOpenOptions
            {
                AllowMultiple = false,
                FileTypeFilter = new[]
                {
                    new FilePickerFileType("Image / Archive / PDF")
                    {
                        Patterns = new[]
                        {
                            "*.jpg","*.jpeg","*.png","*.gif","*.bmp","*.webp","*.tif","*.tiff",
                            "*.zip","*.cbz","*.rar","*.cbr","*.7z","*.cb7","*.tar","*.cbt","*.pdf"
                        }
                    }
                }
            });
            if (files != null && files.Count > 0)
            {
                var p = files[0].TryGetLocalPath();
                if (!string.IsNullOrEmpty(p)) onPicked(p!);
            }
        }
        catch { }
    }

    // Config export/import (#24): save/open paalam-config.lisp (s-expressions).
    public static async void PickSaveConfig(Avalonia.Controls.Window win, string suggestedName,
                                            Action<string> onPicked)
    {
        try
        {
            var file = await win.StorageProvider.SaveFilePickerAsync(new FilePickerSaveOptions
            {
                SuggestedFileName = suggestedName,
                DefaultExtension = "lisp",
                FileTypeChoices = new[]
                {
                    new FilePickerFileType("paalam config") { Patterns = new[] { "*.lisp" } }
                }
            });
            if (file != null)
            {
                var p = file.TryGetLocalPath();
                if (!string.IsNullOrEmpty(p)) onPicked(p!);
            }
        }
        catch { }
    }

    public static async void PickOpenConfig(Avalonia.Controls.Window win, Action<string> onPicked)
    {
        try
        {
            var files = await win.StorageProvider.OpenFilePickerAsync(new FilePickerOpenOptions
            {
                AllowMultiple = false,
                FileTypeFilter = new[]
                {
                    new FilePickerFileType("paalam config") { Patterns = new[] { "*.lisp" } }
                }
            });
            if (files != null && files.Count > 0)
            {
                var p = files[0].TryGetLocalPath();
                if (!string.IsNullOrEmpty(p)) onPicked(p!);
            }
        }
        catch { }
    }

    public static async void PickOpenFolder(Avalonia.Controls.Window win, Action<string> onPicked)
    {
        try
        {
            var folders = await win.StorageProvider.OpenFolderPickerAsync(
                new FolderPickerOpenOptions { AllowMultiple = false });
            if (folders != null && folders.Count > 0)
            {
                var p = folders[0].TryGetLocalPath();
                if (!string.IsNullOrEmpty(p)) onPicked(p!);
            }
        }
        catch { }
    }
}

// (File activation — macOS Finder "Open With" / Dock drop / file-association, #19 — was the
//  Paalam.FileOpenHelper shim; now wired directly in Lisp via %enable-file-open in app.lisp.)

// Pin macOS to Software rendering (no GPU Metal/OpenGl path).
//
// NOTE: this does NOT fix the macOS RenderTimer/-6661 crash (Avalonia #14927), despite the
// original intent. In Avalonia 12 the CVDisplayLink-based RenderTimer is created unconditionally
// in AvaloniaNativePlatform.DoInitialize, independent of RenderingMode, so Software mode cannot
// avoid CVDisplayLink. The -6661 crash is environmental (e.g. launching while the screen is
// locked / lid closed) and is unrelated to this option. We keep Software rendering only because
// it is harmless here and avoids the GPU path; the real -6661 fix would need an IRenderTimer
// fallback (not done). Call only on macOS (this option is a no-op elsewhere).
//
// The generic AppBuilder.With<T> and IList<enum> construction are awkward from Lisp's dotnet:
// FFI, so build them on the C# side.
public static class MacRenderFix
{
    public static Avalonia.AppBuilder UseSoftwareRendering(Avalonia.AppBuilder builder)
    {
        return builder.With(new Avalonia.AvaloniaNativePlatformOptions
        {
            RenderingMode = new[] { Avalonia.AvaloniaNativeRenderingMode.Software }
        });
    }
}

internal static class Program
{
    [STAThread]
    private static void Main()
    {
        try
        {
            // Load Avalonia assemblies into the AppDomain before dotcl resolves types.
            _ = typeof(Avalonia.Application).Assembly;
            _ = typeof(Avalonia.Controls.Window).Assembly;
            _ = typeof(Avalonia.AppBuilderDesktopExtensions).Assembly; // Avalonia.Desktop
            _ = typeof(Avalonia.Themes.Fluent.FluentTheme).Assembly;
            _ = typeof(SharpCompress.Archives.ArchiveFactory).Assembly;
            _ = typeof(PDFtoImage.Conversion).Assembly;
            _ = typeof(SkiaSharp.Heic.HeicDecoder).Assembly;   // #13: HEIC/HEIF decode from Lisp

            DotclHost.Initialize();
            // project-core build (dotcl #307): load the artifacts produced by fully compiling
            // paalam.asd at build time (dotcl.core + dependency fasls + paalam.fasl) in manifest order.
            // LoadFromManifest also handles loading dotcl.core (no LoadCore needed).
            var manifest = Path.Combine(AppContext.BaseDirectory, "dotcl-fasl", "dotcl-deps.txt");
            DotclHost.LoadFromManifest(manifest);

            // Pass the first argument (folder/archive/image). run-paalam blocks until the window closes.
            var cmd = Environment.GetCommandLineArgs();
            object? target = cmd.Length > 1 ? cmd[1] : null;
            DotclHost.Call("RUN-PAALAM", target);
        }
        catch (Exception ex)
        {
            // Log boot exceptions. A signed .app's BaseDirectory may not be writable, so write
            // under the home directory (Console.Error isn't visible on Finder launch).
            try
            {
                var log = Path.Combine(
                    Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
                    "paalam-boot.log");
                File.WriteAllText(log, ex.ToString());
            }
            catch { }
            Console.Error.WriteLine(ex);
            throw;
        }
    }
}

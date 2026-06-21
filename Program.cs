// paalam — Avalonia image viewer (dotcl embedded host, cross-platform)
//
// C# only does boot and assembly pre-loading. UI/logic lives in src/main.lisp.
// Both dotcl/Avalonia are via NuGet only (no dependency on the dotcl repo; #281).

using System;
using System.IO;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using Avalonia.Input;
using Avalonia.Platform.Storage;
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

    // Full-text search (#23 /): 0-based index array of pages containing query. Delegates to PdfText (uses its own bytes).
    public int[] SearchPages(string query) => PdfText.SearchPages(_bytes, query);
    // Highlight rects of hits (page-normalized coords [x,y,w,h,...]; caller multiplies by display dimensions).
    public double[] MatchRects(int pageIndex, string query) => PdfText.MatchRects(_bytes, pageIndex, query);
    // Outline (string of "depth<TAB>page<TAB>title" lines).
    public string Outline() => PdfText.Outline(_bytes);

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

// PDF full-text search (#23 / 's PDF support). PDFtoImage is rendering-only, so we
// P/Invoke the bundled pdfium native directly for text search (FPDFText_Find*). No new
// native dependency, works on all platforms.
// raw P/Invoke can't be declared in Lisp's dotnet: FFI, so it lives in C# (legitimate native interop).
public static class PdfText
{
    private static bool _init;
    private static readonly object _lock = new();
    private const CallingConvention CC = CallingConvention.Cdecl;

    [DllImport("pdfium", CallingConvention = CC)] private static extern void FPDF_InitLibrary();
    [DllImport("pdfium", CallingConvention = CC)] private static extern IntPtr FPDF_LoadMemDocument(IntPtr data, int size, IntPtr password);
    [DllImport("pdfium", CallingConvention = CC)] private static extern void FPDF_CloseDocument(IntPtr doc);
    [DllImport("pdfium", CallingConvention = CC)] private static extern int FPDF_GetPageCount(IntPtr doc);
    [DllImport("pdfium", CallingConvention = CC)] private static extern IntPtr FPDF_LoadPage(IntPtr doc, int index);
    [DllImport("pdfium", CallingConvention = CC)] private static extern void FPDF_ClosePage(IntPtr page);
    [DllImport("pdfium", CallingConvention = CC)] private static extern IntPtr FPDFText_LoadPage(IntPtr page);
    [DllImport("pdfium", CallingConvention = CC)] private static extern void FPDFText_ClosePage(IntPtr textPage);
    [DllImport("pdfium", CallingConvention = CC)] private static extern IntPtr FPDFText_FindStart(IntPtr textPage, ushort[] findWhat, uint flags, int startIndex);
    [DllImport("pdfium", CallingConvention = CC)][return: MarshalAs(UnmanagedType.Bool)] private static extern bool FPDFText_FindNext(IntPtr handle);
    [DllImport("pdfium", CallingConvention = CC)] private static extern void FPDFText_FindClose(IntPtr handle);
    [DllImport("pdfium", CallingConvention = CC)] private static extern int FPDFText_GetSchResultIndex(IntPtr handle);
    [DllImport("pdfium", CallingConvention = CC)] private static extern int FPDFText_GetSchCount(IntPtr handle);
    [DllImport("pdfium", CallingConvention = CC)] private static extern int FPDFText_CountRects(IntPtr textPage, int startIndex, int count);
    [DllImport("pdfium", CallingConvention = CC)] private static extern void FPDFText_GetRect(IntPtr textPage, int rectIndex, out double left, out double top, out double right, out double bottom);
    [DllImport("pdfium", CallingConvention = CC)] private static extern float FPDF_GetPageHeightF(IntPtr page);
    [DllImport("pdfium", CallingConvention = CC)] private static extern float FPDF_GetPageWidthF(IntPtr page);
    [DllImport("pdfium", CallingConvention = CC)] private static extern IntPtr FPDFBookmark_GetFirstChild(IntPtr doc, IntPtr bookmark);
    [DllImport("pdfium", CallingConvention = CC)] private static extern IntPtr FPDFBookmark_GetNextSibling(IntPtr doc, IntPtr bookmark);
    [DllImport("pdfium", CallingConvention = CC)] private static extern uint FPDFBookmark_GetTitle(IntPtr bookmark, byte[] buffer, uint buflen);
    [DllImport("pdfium", CallingConvention = CC)] private static extern IntPtr FPDFBookmark_GetDest(IntPtr doc, IntPtr bookmark);
    [DllImport("pdfium", CallingConvention = CC)] private static extern int FPDFDest_GetDestPageIndex(IntPtr doc, IntPtr dest);

    // Return the outline (PDF bookmarks) as a string of "depth<TAB>page<TAB>title" lines (depth-first). "" if none.
    // Use a delimited string so it's easy to handle from dotcl (Lisp splits by line).
    public static string Outline(byte[] bytes)
    {
        if (bytes == null || bytes.Length == 0) return "";
        lock (_lock)
        {
            if (!_init) { FPDF_InitLibrary(); _init = true; }
            var pin = GCHandle.Alloc(bytes, GCHandleType.Pinned);
            IntPtr doc = IntPtr.Zero;
            try
            {
                doc = FPDF_LoadMemDocument(pin.AddrOfPinnedObject(), bytes.Length, IntPtr.Zero);
                if (doc == IntPtr.Zero) return "";
                var sb = new System.Text.StringBuilder();
                var buf = new byte[2048];
                void Walk(IntPtr bm, int depth)
                {
                    int guard = 0;
                    while (bm != IntPtr.Zero && guard++ < 5000)
                    {
                        uint n = FPDFBookmark_GetTitle(bm, buf, (uint)buf.Length);
                        string title = n > 2
                            ? System.Text.Encoding.Unicode.GetString(buf, 0, (int)Math.Min(n, buf.Length)).TrimEnd('\0')
                            : "";
                        IntPtr dest = FPDFBookmark_GetDest(doc, bm);
                        int page = dest != IntPtr.Zero ? FPDFDest_GetDestPageIndex(doc, dest) : -1;
                        sb.Append(depth).Append('\t').Append(page).Append('\t')
                          .Append(title.Replace('\t', ' ').Replace('\n', ' ').Replace('\r', ' ')).Append('\n');
                        Walk(FPDFBookmark_GetFirstChild(doc, bm), depth + 1);
                        bm = FPDFBookmark_GetNextSibling(doc, bm);
                    }
                }
                Walk(FPDFBookmark_GetFirstChild(doc, IntPtr.Zero), 0);
                return sb.ToString();
            }
            finally { if (doc != IntPtr.Zero) FPDF_CloseDocument(doc); pin.Free(); }
        }
    }

    // Return the rects of query's first hit on pageIndex, as normalized coords relative to
    // page dimensions (0..1, top-left origin), as [x,y,w,h, x,y,w,h, ...] (multiple rects if
    // multi-line). Empty array if none.
    // Since normalized, the caller just multiplies by the page image's display dimensions (DPI/metadata-independent).
    public static double[] MatchRects(byte[] bytes, int pageIndex, string query)
    {
        if (bytes == null || bytes.Length == 0 || string.IsNullOrEmpty(query)) return Array.Empty<double>();
        lock (_lock)
        {
            if (!_init) { FPDF_InitLibrary(); _init = true; }
            var pin = GCHandle.Alloc(bytes, GCHandleType.Pinned);
            IntPtr doc = IntPtr.Zero, page = IntPtr.Zero, tp = IntPtr.Zero, sh = IntPtr.Zero;
            try
            {
                doc = FPDF_LoadMemDocument(pin.AddrOfPinnedObject(), bytes.Length, IntPtr.Zero);
                if (doc == IntPtr.Zero) return Array.Empty<double>();
                page = FPDF_LoadPage(doc, pageIndex);
                if (page == IntPtr.Zero) return Array.Empty<double>();
                tp = FPDFText_LoadPage(page);
                if (tp == IntPtr.Zero) return Array.Empty<double>();
                sh = FPDFText_FindStart(tp, ToUtf16(query), 0, 0);
                if (sh == IntPtr.Zero || !FPDFText_FindNext(sh)) return Array.Empty<double>();
                int start = FPDFText_GetSchResultIndex(sh);
                int cnt = FPDFText_GetSchCount(sh);
                int nr = FPDFText_CountRects(tp, start, cnt);
                double pw = FPDF_GetPageWidthF(page);
                double ph = FPDF_GetPageHeightF(page);   // points (1/72in). PDF origin is bottom-left.
                if (pw <= 0 || ph <= 0) return Array.Empty<double>();
                var outv = new List<double>(nr * 4);
                for (int i = 0; i < nr; i++)
                {
                    FPDFText_GetRect(tp, i, out double l, out double t, out double r, out double b);
                    outv.Add(l / pw);                 // x ratio
                    outv.Add((ph - t) / ph);          // y ratio (flipped vertically: top is the larger y)
                    outv.Add((r - l) / pw);           // w ratio
                    outv.Add((t - b) / ph);           // h ratio
                }
                return outv.ToArray();
            }
            finally
            {
                if (sh != IntPtr.Zero) FPDFText_FindClose(sh);
                if (tp != IntPtr.Zero) FPDFText_ClosePage(tp);
                if (page != IntPtr.Zero) FPDF_ClosePage(page);
                if (doc != IntPtr.Zero) FPDF_CloseDocument(doc);
                pin.Free();
            }
        }
    }

    // Return a 0-based index array of pages containing query (case-insensitive, in order of appearance).
    // FPDF_LoadMemDocument keeps referencing the buffer, so pin the byte[] until doc is closed.
    public static int[] SearchPages(byte[] bytes, string query)
    {
        if (bytes == null || bytes.Length == 0 || string.IsNullOrEmpty(query))
            return Array.Empty<int>();
        lock (_lock)
        {
            if (!_init) { FPDF_InitLibrary(); _init = true; }   // OK to share with PDFtoImage (init treated as idempotent)
            var pin = GCHandle.Alloc(bytes, GCHandleType.Pinned);
            IntPtr doc = IntPtr.Zero;
            try
            {
                doc = FPDF_LoadMemDocument(pin.AddrOfPinnedObject(), bytes.Length, IntPtr.Zero);
                if (doc == IntPtr.Zero) return Array.Empty<int>();
                var findWhat = ToUtf16(query);
                var hits = new List<int>();
                int n = FPDF_GetPageCount(doc);
                for (int i = 0; i < n; i++)
                {
                    IntPtr page = FPDF_LoadPage(doc, i);
                    if (page == IntPtr.Zero) continue;
                    IntPtr tp = FPDFText_LoadPage(page);
                    if (tp != IntPtr.Zero)
                    {
                        IntPtr sh = FPDFText_FindStart(tp, findWhat, 0, 0);   // flags 0 = case-insensitive
                        if (sh != IntPtr.Zero)
                        {
                            if (FPDFText_FindNext(sh)) hits.Add(i);
                            FPDFText_FindClose(sh);
                        }
                        FPDFText_ClosePage(tp);
                    }
                    FPDF_ClosePage(page);
                }
                return hits.ToArray();
            }
            finally
            {
                if (doc != IntPtr.Zero) FPDF_CloseDocument(doc);
                pin.Free();
            }
        }
    }

    // FPDF_WIDESTRING = UTF-16LE null-terminated.
    private static ushort[] ToUtf16(string s)
    {
        var a = new ushort[s.Length + 1];
        for (int i = 0; i < s.Length; i++) a[i] = s[i];
        a[s.Length] = 0;
        return a;
    }
}

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

// Workaround for the macOS RenderTimer/-6661 issue (Avalonia #14927). The CVDisplayLink-based
// RenderTimer fails at CreateAvaloniaNative/RegisterTick early in startup (reproduces under
// conditions like a closed lid, external-display-only, or races depending on the launch path).
// The public API can't directly replace the RenderTimer, so pin it to Software rendering that
// doesn't use the GPU path (Metal/OpenGl), avoiding CVDisplayLink/Metal initialization itself.
// The generic AppBuilder.With<T> and IList<enum> construction are awkward from Lisp's dotnet:
// FFI, so build them on the C# side. Call only on macOS (an option that is ineffective on other OSes).
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

// paalam — Avalonia 画像ビューア (dotcl 組み込みホスト, クロスプラット)
//
// C# は boot とアセンブリ先読みだけ。UI/ロジックは src/main.lisp 側。
// dotcl/Avalonia とも NuGet 経由のみ (dotcl repo 非依存; #281)。

using System;
using System.IO;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using Avalonia.Input;
using Avalonia.Platform.Storage;
using DotCL;

namespace Paalam;

// （旧 Paalam.Imaging.BitmapFromStream シムは撤去。dotcl-internal #294 が
//  DotCL.Runtime 0.1.12 で修正され、Lisp から (dotnet:new Bitmap stream) が
//  Bitmap(Stream) を正しく選べるようになったため。）

// PDF を PDFium (PDFtoImage) でページ毎にラスタライズして Avalonia Bitmap にする。
// PDFium/SkiaSharp interop は C# 側に閉じ込める (Lisp からは PageCount/RenderPage だけ叩く)。
public sealed class PdfDoc
{
    private readonly byte[] _bytes;
    public PdfDoc(string path) => _bytes = System.IO.File.ReadAllBytes(path);
    public int PageCount => PDFtoImage.Conversion.GetPageCount(_bytes);
    // dpi はラスタライズ解像度 (Lisp の *pdf-dpi*)。4K では 150 だと拡大されて粗いので
    // 既定 300、設定で可変。高いほど精細だがメモリ/時間が増える。
    public Avalonia.Media.Imaging.Bitmap RenderPage(int index, int dpi)
    {
        var opts = new PDFtoImage.RenderOptions(Dpi: dpi,
                       BackgroundColor: SkiaSharp.SKColors.White);
        using var sk = PDFtoImage.Conversion.ToImage(_bytes, index, null, opts);
        using var data = sk.Encode(SkiaSharp.SKEncodedImageFormat.Png, 90);
        using var ms = new System.IO.MemoryStream(data.ToArray());
        return new Avalonia.Media.Imaging.Bitmap(ms);
    }

    // 全文検索 (#23 /): query を含むページの 0-based index 配列。PdfText に委譲 (自前の bytes を使う)。
    public int[] SearchPages(string query) => PdfText.SearchPages(_bytes, query);

    // 素材掃き出し用: ページを PNG バイト列で返す。
    public byte[] RenderPagePng(int index, int dpi)
    {
        var opts = new PDFtoImage.RenderOptions(Dpi: dpi,
                       BackgroundColor: SkiaSharp.SKColors.White);
        using var sk = PDFtoImage.Conversion.ToImage(_bytes, index, null, opts);
        using var data = sk.Encode(SkiaSharp.SKEncodedImageFormat.Png, 90);
        return data.ToArray();
    }
}

// PDF 全文検索 (#23 / の PDF 対応)。PDFtoImage は描画専用なので、同梱の pdfium ネイティブを
// 直接 P/Invoke してテキスト検索する (FPDFText_Find*)。新規ネイティブ依存なし・全プラット対応。
// raw P/Invoke は Lisp の dotnet: FFI では宣言できないので C# に置く (正当なネイティブ interop)。
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

    // query を含むページの 0-based index 配列を返す (大文字小文字無視・出現順)。
    // FPDF_LoadMemDocument はバッファを参照し続けるので、doc を閉じるまで byte[] を pin し続ける。
    public static int[] SearchPages(byte[] bytes, string query)
    {
        if (bytes == null || bytes.Length == 0 || string.IsNullOrEmpty(query))
            return Array.Empty<int>();
        lock (_lock)
        {
            if (!_init) { FPDF_InitLibrary(); _init = true; }   // PDFtoImage と共有 OK (init は冪等運用)
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
                        IntPtr sh = FPDFText_FindStart(tp, findWhat, 0, 0);   // flags 0 = 大小無視
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

    // FPDF_WIDESTRING = UTF-16LE null 終端。
    private static ushort[] ToUtf16(string s)
    {
        var a = new ushort[s.Length + 1];
        for (int i = 0; i < s.Length; i++) a[i] = s[i];
        a[s.Length] = 0;
        return a;
    }
}

// ドラッグ&ドロップ配線を C# 側に封じ込める (Avalonia DnD はルーテッドイベントで
// Lisp から直接扱いにくいため)。Lisp は onDrop(path) を受け取るだけ。
public static class DragDropHelper
{
    // onHighlight: DnD のドロップ可否フィードバックを Lisp 側に通知する (見た目は Lisp が描く)。
    // ファイルがドラッグされて来たら true、出たら/ドロップしたら false。
    public static void Enable(Avalonia.Controls.Window win, Action<string> onDrop, Action<bool> onHighlight)
    {
        // Avalonia 12: DnD は IDataTransfer + DataFormat (旧 IDataObject/DataFormats は廃止)。
        DragDrop.SetAllowDrop(win, true);
        win.AddHandler(DragDrop.DragOverEvent, (s, e) =>
        {
            bool hasFiles = e.DataTransfer.Contains(DataFormat.File);
            e.DragEffects = hasFiles ? DragDropEffects.Copy : DragDropEffects.None;
            onHighlight(hasFiles);
        });
        win.AddHandler(DragDrop.DragLeaveEvent, (s, e) => onHighlight(false));
        win.AddHandler(DragDrop.DropEvent, (s, e) =>
        {
            onHighlight(false);
            var files = e.DataTransfer.TryGetFiles();
            if (files == null) return;
            foreach (var f in files)
            {
                var p = f.TryGetLocalPath();
                if (!string.IsNullOrEmpty(p)) { onDrop(p!); break; }
            }
        });
    }

    // 素材ペイン用: ドロップされた全パスを onDrop に渡す。e.Handled=true で
    // ウィンドウ側の「本を開く」DnD と区別 (ペインに落としたら素材に追加)。
    public static void EnableClipDrop(Avalonia.Controls.Control pane, Action<string> onDrop, Action<bool> onHighlight)
    {
        DragDrop.SetAllowDrop(pane, true);
        pane.AddHandler(DragDrop.DragOverEvent, (s, e) =>
        {
            bool hasFiles = e.DataTransfer.Contains(DataFormat.File);
            e.DragEffects = hasFiles ? DragDropEffects.Copy : DragDropEffects.None;
            onHighlight(hasFiles);
            e.Handled = true;
        });
        pane.AddHandler(DragDrop.DragLeaveEvent, (s, e) => { onHighlight(false); e.Handled = true; });
        pane.AddHandler(DragDrop.DropEvent, (s, e) =>
        {
            onHighlight(false);
            var files = e.DataTransfer.TryGetFiles();
            if (files != null)
                foreach (var f in files)
                {
                    var p = f.TryGetLocalPath();
                    if (!string.IsNullOrEmpty(p)) onDrop(p!);
                }
            e.Handled = true;
        });
    }
}

// 保存ダイアログ (素材掃き出し先の選択)。async は UI スレッドで進み、確定後に
// onPicked(path) を呼ぶ (UI を固めない)。
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
        catch { /* キャンセル等は無視 */ }
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
                    new FilePickerFileType("画像 / 書庫 / PDF")
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

internal static class Program
{
    [STAThread]
    private static void Main()
    {
        try
        {
            // dotcl の型解決前に Avalonia アセンブリを AppDomain にロードしておく。
            _ = typeof(Avalonia.Application).Assembly;
            _ = typeof(Avalonia.Controls.Window).Assembly;
            _ = typeof(Avalonia.AppBuilderDesktopExtensions).Assembly; // Avalonia.Desktop
            _ = typeof(Avalonia.Themes.Fluent.FluentTheme).Assembly;
            _ = typeof(SharpCompress.Archives.ArchiveFactory).Assembly;
            _ = typeof(PDFtoImage.Conversion).Assembly;

            DotclHost.Initialize();
            // project-core build (dotcl #307): build 時に paalam.asd を精コンパイルした
            // 成果物 (dotcl.core + 依存 fasl + paalam.fasl) を manifest 順にロードする。
            // LoadFromManifest が dotcl.core のロードも兼ねる (LoadCore 不要)。
            var manifest = Path.Combine(AppContext.BaseDirectory, "dotcl-fasl", "dotcl-deps.txt");
            DotclHost.LoadFromManifest(manifest);

            // 第 1 引数 (フォルダ/アーカイブ/画像) を渡す。run-paalam は窓が閉じるまで blocking。
            var cmd = Environment.GetCommandLineArgs();
            object? target = cmd.Length > 1 ? cmd[1] : null;
            DotclHost.Call("RUN-PAALAM", target);
        }
        catch (Exception ex)
        {
            File.WriteAllText(
                Path.Combine(AppContext.BaseDirectory, "paalam-boot.log"), ex.ToString());
            Console.Error.WriteLine(ex);
            throw;
        }
    }
}

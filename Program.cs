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
    // ヒット箇所のハイライト矩形 (ページ正規化座標 [x,y,w,h,...]; 呼び側で表示寸法を掛ける)。
    public double[] MatchRects(int pageIndex, string query) => PdfText.MatchRects(_bytes, pageIndex, query);
    // 目次 ("depth<TAB>page<TAB>title" 行の文字列)。
    public string Outline() => PdfText.Outline(_bytes);

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

    // 目次 (PDF bookmark) を "depth<TAB>page<TAB>title" 行の文字列で返す (深さ優先)。無ければ ""。
    // dotcl から扱いやすいよう区切り文字列にする (Lisp 側で行分割)。
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

    // pageIndex 上の query 最初のヒットの矩形群を、ページ寸法に対する正規化座標 (0..1, 左上原点)
    // で [x,y,w,h, x,y,w,h, ...] で返す (複数行なら複数矩形)。無ければ空配列。
    // 正規化なので呼び側でページ画像の表示寸法を掛ければよい (DPI/メタデータ非依存)。
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
                double ph = FPDF_GetPageHeightF(page);   // 点 (1/72in)。PDF は原点が左下。
                if (pw <= 0 || ph <= 0) return Array.Empty<double>();
                var outv = new List<double>(nr * 4);
                for (int i = 0; i < nr; i++)
                {
                    FPDFText_GetRect(tp, i, out double l, out double t, out double r, out double b);
                    outv.Add(l / pw);                 // x 比
                    outv.Add((ph - t) / ph);          // y 比 (上下反転: top が大きい y)
                    outv.Add((r - l) / pw);           // w 比
                    outv.Add((t - b) / ph);           // h 比
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

// macOS の Finder「このアプリで開く」/ Dock へのドロップ / 関連付け起動で渡される
// ファイルを受ける (#19)。これらは argv ではなく Apple Event 経由で来るので、
// Avalonia の IActivatableLifetime.Activated (ActivationKind.File) を購読する。
// ジェネリック TryGetFeature<T> と FileActivatedEventArgs のキャストは Lisp の
// dotnet: FFI では扱いづらいので C# シムに閉じ込め、Lisp へは文字列パスだけ渡す。
public static class FileOpenHelper
{
    // app: Avalonia.Application (Paalam.App)。onFile: 開くべきローカルパスを 1 件ずつ通知。
    public static void Enable(Avalonia.Application app, Action<string> onFile)
    {
        try
        {
            // Avalonia 12 の TryGetFeature は非ジェネリック (Type を取る) 版。
            var life = app.TryGetFeature(
                typeof(Avalonia.Controls.ApplicationLifetimes.IActivatableLifetime))
                as Avalonia.Controls.ApplicationLifetimes.IActivatableLifetime;
            if (life == null) return;   // 非対応プラットフォーム
            life.Activated += (s, e) =>
            {
                if (e is Avalonia.Controls.ApplicationLifetimes.FileActivatedEventArgs fa)
                {
                    foreach (var item in fa.Files)
                    {
                        var p = item.TryGetLocalPath();
                        if (!string.IsNullOrEmpty(p)) onFile(p!);
                    }
                }
            };
        }
        catch { /* 機能が無い環境では黙ってスキップ */ }
    }
}

// macOS の RenderTimer/-6661 回避 (Avalonia #14927)。CVDisplayLink ベースの
// RenderTimer が起動初期に CreateAvaloniaNative/RegisterTick で失敗する
// (蓋を閉じた・外部ディスプレイのみ・起動経路によるレース等の条件で再現)。
// 公開 API では RenderTimer を直接差し替えられないので、GPU パス (Metal/OpenGl) を
// 使わない Software レンダリングに固定して CVDisplayLink/Metal の初期化自体を避ける。
// ジェネリックな AppBuilder.With<T> と IList<enum> 構築は Lisp の dotnet: FFI から
// 扱いづらいので C# 側で組む。macOS でのみ呼ぶこと (他 OS には無効なオプション)。
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
            // boot 例外はログに残す。署名済み .app の BaseDirectory は書けないことがあるので
            // ホーム配下に出す (Console.Error は Finder 起動では見えない)。
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

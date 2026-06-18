// paalam — Avalonia 画像ビューア (dotcl 組み込みホスト, クロスプラット)
//
// C# は boot とアセンブリ先読みだけ。UI/ロジックは src/main.lisp 側。
// dotcl/Avalonia とも NuGet 経由のみ (dotcl repo 非依存; #281)。

using System;
using System.IO;
using Avalonia.Input;
using Avalonia.Platform.Storage;
using DotCL;

namespace Paalam;

// dotcl の dotnet:new はコンストラクタ多重定義解決が弱く (プリミティブ型しか
// スコアしない)、Bitmap(string) と Bitmap(Stream) を区別できず Stream を string に
// 変換しようとして失敗する。1 メソッドだけの曖昧さ回避シムで Stream ctor を確実に呼ぶ。
// (dotcl-internal の overload 解決改善が入れば不要)
public static class Imaging
{
    public static Avalonia.Media.Imaging.Bitmap BitmapFromStream(System.IO.Stream s)
        => new Avalonia.Media.Imaging.Bitmap(s);
}

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
            DotclHost.LoadCore(DotclHost.FindCore()
                ?? throw new InvalidOperationException("dotcl.core が見つかりません"));
            // dist = 精コンパイル済み paalam.fasl (起動が速い・ソース非平文)、
            // dev = main.lisp (デバッグ時の行番号が出る)。出力にある方を load (#22 Phase 2)。
            var fasl = Path.Combine(AppContext.BaseDirectory, "paalam.fasl");
            DotclHost.LoadLispFile(File.Exists(fasl)
                ? fasl
                : Path.Combine(AppContext.BaseDirectory, "main.lisp"));

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

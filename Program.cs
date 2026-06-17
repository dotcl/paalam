// paalam — Avalonia 画像ビューア (dotcl 組み込みホスト, クロスプラット)
//
// C# は boot とアセンブリ先読みだけ。UI/ロジックは src/main.lisp 側。
// dotcl/Avalonia とも NuGet 経由のみ (dotcl repo 非依存; #281)。

using System;
using System.IO;
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

            DotclHost.Initialize();
            DotclHost.LoadCore(DotclHost.FindCore()
                ?? throw new InvalidOperationException("dotcl.core が見つかりません"));
            DotclHost.LoadLispFile(Path.Combine(AppContext.BaseDirectory, "main.lisp"));

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

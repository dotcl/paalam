// paalam — WPF 画像ビューア (dotcl 組み込みホスト)
//
// C# は起動配線だけ。UI / ロジックは src/main.lisp (Common Lisp) 側。
// dotcl repo には依存せず DotCL.Runtime NuGet だけで組み込む (dotcl-internal #281)。

using System.IO;
using System.Windows;
using DotCL;

namespace Paalam;

internal static class Program
{
    [STAThread]
    private static void Main()
    {
        try
        {
            // WPF 本体を先にロード (dotcl 側が単純名で型解決できるように)。
            _ = typeof(Application).Assembly;

            // dotcl ランタイム起動: Initialize → core ロード → main.lisp ロード。
            DotclHost.Initialize();
            var core = DotclHost.FindCore()
                ?? throw new InvalidOperationException(
                    "dotcl.core が見つかりません (paalam.exe と同じ場所に配置されるはず)");
            DotclHost.LoadCore(core);

            var mainLisp = Path.Combine(AppContext.BaseDirectory, "main.lisp");
            DotclHost.LoadLispFile(mainLisp);

            // 第 1 引数 (フォルダ/アーカイブ/画像) を Lisp に渡す。無ければ null。
            var cmd = Environment.GetCommandLineArgs();
            object? target = cmd.Length > 1 ? cmd[1] : null;

            // Lisp 側 build-app が Application を組んで返す。STAThread Main 上で Run。
            var result = DotclHost.Call("BUILD-APP", target);
            if (result is LispDotNetObject obj && obj.Value is Application app)
                app.Run();
            else
                throw new InvalidOperationException(
                    $"BUILD-APP が System.Windows.Application を返しませんでした (got: {result})");
        }
        catch (Exception ex)
        {
            // WinExe はコンソールを持たないので boot trace/例外はログに残す。
            var log = Path.Combine(AppContext.BaseDirectory, "paalam-boot.log");
            File.WriteAllText(log, ex.ToString());
            throw;
        }
    }
}

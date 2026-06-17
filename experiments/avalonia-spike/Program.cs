// Avalonia spike host — dotcl が Avalonia をクロスプラットで起動できるかの実証。
// C# は boot とアセンブリ先読みだけ。UI/起動ロジックは app.lisp 側。

using System;
using System.IO;
using DotCL;

namespace AvSpike;

internal static class Program
{
    [STAThread]
    private static void Main()
    {
        try
        {
            // dotcl の型解決前に Avalonia アセンブリを AppDomain にロードしておく。
            _ = typeof(Avalonia.Application).Assembly;                 // Avalonia.Base/Controls
            _ = typeof(Avalonia.Controls.Window).Assembly;            // Avalonia.Controls
            _ = typeof(Avalonia.AppBuilderDesktopExtensions).Assembly; // Avalonia.Desktop
            _ = typeof(Avalonia.Themes.Fluent.FluentTheme).Assembly;  // Avalonia.Themes.Fluent

            DotclHost.Initialize();
            DotclHost.LoadCore(DotclHost.FindCore()
                ?? throw new InvalidOperationException("dotcl.core が見つかりません"));
            DotclHost.LoadLispFile(Path.Combine(AppContext.BaseDirectory, "app.lisp"));

            // run-avalonia は窓が閉じる (or autoclose) まで blocking。
            DotclHost.Call("RUN-AVALONIA");
        }
        catch (Exception ex)
        {
            File.WriteAllText(
                Path.Combine(AppContext.BaseDirectory, "avspike-boot.log"), ex.ToString());
            Console.Error.WriteLine(ex);
            throw;
        }
    }
}

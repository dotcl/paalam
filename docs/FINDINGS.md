# paalam — dotcl + WPF 実現可能性 調査メモ

NeeView 風の画像ビューアを dotcl (= .NET 上の Common Lisp) で作る前提の
技術検証結果。`experiments/` の 2 スクリプトで実証済み (2026-06-17)。

## 結論

**dotcl の素のインストール版 (0.1.8) だけで、WPF の可視ウィンドウ表示まで
到達できる。リビルドも C# ホストプロジェクトも不要。**

検証済みの経路 (全 PASS):

1. WPF アセンブリ群の load
2. `BitmapImage` による画像デコード (WIC)
3. STA スレッド上での `Window` + `Image` 構築 (Dispatcher 初期化)
4. 可視ウィンドウ `Show` + `Dispatcher.Run` メッセージポンプ + イベント処理
   (`Closed`, `DispatcherTimer`) + クリーン終了

## ハマりどころ (重要な知見)

### 1. WPF DLL は WindowsDesktop.App から *フルパス* で load する

`(dotnet:load-assembly "WindowsBase")` のような **単純名は版不一致** で失敗する:

```
Could not load file or assembly 'WindowsBase, Version=10.0.0.0 ...'
The located assembly's manifest definition does not match ... (0x80131040)
```

正解は共有ランタイムフォルダの DLL をフルパス指定:

```
C:/Program Files/dotnet/shared/Microsoft.WindowsDesktop.App/<version>/WindowsBase.dll
```

バージョンは `(directory ".../Microsoft.WindowsDesktop.App/*/")` で動的検出する。
読む順: WindowsBase → PresentationCore → PresentationFramework → System.Xaml。

（補足: WinForms (`System.Windows.Forms`) は単純名 load で動く。WPF だけ
フルパスが要る。両者とも実体は WindowsDesktop.App ランタイム。
このマシンには 10.0.5 / 10.0.7 がインストール済み。）

### 2. WPF は STA スレッド必須 — Lisp から自作できる

インストール版 0.1.8 には `dotnet:ui-invoke` が**まだ無い**
(ソースツリーには有るが WinForms メッセージループ前提)。
しかし STA スレッドは Lisp から直接作れる:

```lisp
(let ((th (dotnet:new "System.Threading.Thread"
            (lambda () ... WPF をここで構築 ...))))   ; lambda → ThreadStart marshalling
  (dotnet:invoke th "SetApartmentState"
                 (dotnet:static "System.Threading.ApartmentState" "STA"))
  (dotnet:invoke th "Start"))
```

メッセージポンプは `(dotnet:static "System.Windows.Threading.Dispatcher" "Run")`。
ウィンドウの `Closed` イベントで `(dotnet:invoke dispatcher "InvokeShutdown")`
を呼べば `Dispatcher.Run` から抜ける。

### 3. dotcl は位置引数を「ロードすべきファイル」として扱う

`dotcl script.lisp foo.png` は **foo.png も lisp ソースとして load** しようとし
`Unbound variable: ?PNG` 等で落ちる。データ引数は環境変数で渡す
(例: `DOTCL_AUTOCLOSE_MS`)。`dotcl:command-line-arguments` で全 argv は取れる。

## 使った dotcl API

`dotnet:load-assembly` / `dotnet:new` / `dotnet:invoke` / `dotnet:static` /
`dotnet:add-event` (イベント購読; lambda が handler になる) /
`dotcl:command-line-arguments`。`dotnet:define-class` は今回未使用だが、
カスタム Window サブクラス (キー入力 override 等) が必要になれば使う。

## 実証スクリプト

- `experiments/wpf-smoke.lisp` — Tier1〜3 のヘッドレス検証
- `experiments/window-show.lisp` — 実ウィンドウ表示 (Uniform フィット)。
  `DOTCL_AUTOCLOSE_MS=1500` で自動クローズ動作確認。

## 配布形態: WPF host + DotCL.Runtime NuGet (検証済み 2026-06-17)

**paalam は dotcl repo に依存せず `DotCL.Runtime` NuGet 参照だけで配布物を作る**
(= dotcl-internal #281 の standalone サンプルの実体)。端から端まで実証済み:
`paalam.exe` → dotcl 起動 → `dotcl.core` ロード → `main.lisp` ロード →
`BUILD-APP` が WPF Application+Window を生成・表示 → `app.Run()` → クリーン終了。

構成:
- `paalam.csproj` — `net10.0-windows` / `UseWPF=true` / `WinExe` /
  `<PackageReference Include="DotCL.Runtime" Version="0.1.10" />` のみ。
- `Program.cs` — `[STAThread] Main`: `typeof(Application).Assembly` で WPF を先読み →
  `DotclHost.Initialize/LoadCore(FindCore())/LoadLispFile("main.lisp")` →
  `DotclHost.Call("BUILD-APP")` の戻り (`LispDotNetObject.Value` = `Application`) を `Run()`。
- `src/main.lisp` — 定義のみ。`build-app` (host 入口) / `build-main-window` / `ensure-wpf`。
- ツールチェーン: `.config/dotnet-tools.json` で dotcl 0.1.10 を local tool ピン留め
  (clone → `dotnet tool restore` で誰でも同一 dotcl)。

host 経路では WPF が **単純名で解決**でき (Desktop ランタイム上)、**STA/Dispatcher は
`Application.Run()` が提供**するので、上記「フルパス load / 自作 STA」の罠は
`dotcl src/main.lisp` スクリプト実行時のみ。`main.lisp` は両対応 (`ensure-wpf` が吸収)。

### host 固有の罠

- **DotCL.Runtime 0.1.10 の `dotcl.core` が出力にコピーされない**。nupkg の contentFiles が
  `buildAction="Content"` のみで `copyToOutput="true"` 不在のため、`FindCore()` が
  `baseDir/dotcl.core` を見つけられず boot 失敗。回避策 (csproj):
  ```xml
  <PackageReference Include="DotCL.Runtime" Version="0.1.10" GeneratePathProperty="true" />
  <None Include="$(PkgDotCL_Runtime)\contentFiles\any\net10.0\dotcl.core"
        Link="dotcl.core" CopyToOutputDirectory="PreserveNewest" Visible="false" />
  ```
  → パッケージ側を直すべき #281 ドッグフード案件。
- **WPF Application は 1 AppDomain 1 個**。`ensure-wpf` の probe で `(dotnet:new "...Application")`
  すると build-app の本生成と衝突。probe は `(dotnet:static "...Application" "Current")` で
  型解決だけにし、build-app は `Current` があれば再利用する。
- **WinExe はコンソール無し**。boot trace / 例外は `paalam-boot.log` に書く。
- 動作確認は `DOTCL_AUTOCLOSE_MS=2500 paalam.exe` (DispatcherTimer で自動 Shutdown → EXIT 0)。

### 配布コマンド

- フレームワーク依存: `dotnet publish -c Release -o dist`
  → `dist/` を zip。実行側に .NET 10 Desktop ランタイム要。
- 自己完結 (前提不要・サイズ大): `dotnet publish -c Release -r win-arm64 --self-contained -o dist`。
- NativeAOT は dotcl が実行時 Reflection.Emit するため**不可**。

## 次の段階 (MVP ロードマップ)

1. ✅ WPF 経路の実証 + ✅ WPF host 配布経路の実証 (本メモ)
2. ✅ book 抽象 (directory/zip backend, 自然順) + ページ送り
   (←/→/PageUp/Down/Space/Home/End)。注: WPF Key.ToString は PageDown→"Next"、
   PageUp→"PageUp" と非対称なので handler は両名を見る。単一画像は親フォルダを
   本にして初期ページに (NeeView 挙動)。
3. Fit モード (幅/高/全体) + ホイールズーム + ドラッグパン
4. ZIP 書庫を「本」として開く (`System.IO.Compression`, 標準同梱)
5. 見開き 2 ページ表示 + 綴じ方向 (右/左)
6. 本棚 (フォルダツリー) + サムネイル + 履歴 + ブックマーク
7. (拡張) RAR/7z = SharpCompress, PDF = Docnet, WebP/AVIF = SkiaSharp (NuGet)

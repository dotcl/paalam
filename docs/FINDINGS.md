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

## 本体を Avalonia に pivot (2026-06-17) — 現在の primary

**paalam 本体は WPF から Avalonia (OSS, MIT, クロスプラット) に載せ替え済み。**
`paalam.csproj` は `net10.0` (-windows 不要) + Avalonia 12.0.4 + DotCL.Runtime のみ。
`book-*` 抽象はそのまま流用し、UI 層 (画像ロード / Window / KeyDown / 起動) だけ
Avalonia 化した。DIR / ZIP 両 backend で起動・ページ送り・配布 (dotnet publish) を確認 (EXIT 0)。
WPF 版はコミット履歴 (e360798〜abea19e) と `experiments/wpf-smoke|window-show.lisp` に残る。

Avalonia 固有の追加摩擦:
- **`Bitmap(string)` と `Bitmap(Stream)` を dotnet:new が区別できない** (overload 解決が
  ラップ引数の代入可能性を見ない) → C# シム `Paalam.Imaging.BitmapFromStream(Stream)`
  経由で zip エントリを読む。dotcl-internal #294。
- dev 反復: Avalonia は host 必須なので script-mode の高速反復は無く、`dotnet run` が dev ループ。

### スパイク (核心実証) — `experiments/avalonia-spike/`

- `dotnet:static-generic` で `AppBuilder.Configure<App>()` を駆動 → **ジェネリック起動 OK**
- ネイティブ Skia (SkiaSharp/HarfBuzz) は csproj+RID で自動解決 (cross-platform native 同梱)
- `dotnet:define-class` で `Avalonia.Application` 派生を emit
- `Window` + `Image` + `Bitmap(path)` 表示、`StartWithClassicDesktopLifetime` 完走 → EXIT 0
- **`net10.0` (`-windows` 不要)・STA 不要・Windows Desktop ランタイム非依存** → #281 に WPF より素直

Avalonia 固有の罠 (下記 #290/#292/#293 は **dotcl 0.1.11 で解決済み**。0.1.11 の
DotCL.Runtime.targets が dotcl.core と contrib を出力に自動コピーするので、main.lisp は
`(require :dotnet-class)`/`(require :dotcl-thread)` で解決し、FluentTheme も引数なしで作れる。
履歴として残す):
- **contrib (define-class) が DotCL.Runtime NuGet に無い** (〜0.1.10) → vendor して `load`
  (`*package*`=CL-USER で読むと `self` 等が利用側と一致)。dotcl-internal #293 (0.1.11 解決)。
- **Avalonia 12 `FluentTheme` は引数なし ctor 無し** (`Uri baseUri=null`)。`dotnet:new` 0 引数版
  (Activator) では作れない → `(dotnet:new "...FluentTheme" nil)` と省略可引数を明示。
  dotcl-internal #292 (0.1.11 で引数なし可)。画像表示自体にテーマは不要。
- Bitmap は `Avalonia.Media.Imaging.Bitmap` (WPF の `BitmapImage` と別型) → book-load-image は
  backend 依存。WPF/Avalonia を同居させるなら画像ロードを backend ごとに分ける。

判断: クロス化は **XPF (有償) ではなく素の Avalonia** で行う。`book-*` 抽象は GUI 非依存
なので、載せ替えで触るのは build-main-window / %on-key / 画像ロードの薄い層のみ。

## 次の段階 (MVP ロードマップ)

1. ✅ WPF 経路の実証 + ✅ WPF host 配布経路の実証 (本メモ)
2. ✅ book 抽象 (directory/zip backend, 自然順) + ページ送り
   (←/→/PageUp/Down/Space/Home/End)。注: WPF Key.ToString は PageDown→"Next"、
   PageUp→"PageUp" と非対称なので handler は両名を見る。単一画像は親フォルダを
   本にして初期ページに (NeeView 挙動)。
3. ✅ ZIP/CBZ 書庫を「本」として開く (`System.IO.Compression`, 標準同梱)
4. ✅ 見開き 2 ページ表示 + 綴じ方向 (右/左)。Avalonia の 2 列 Grid に Image を 2 枚
   中央で突き合わせ。`compute-spreads` が *spread* / *first-page-single* に従い
   見開き単位へ分割 (表紙単独・奇数末尾も処理)。単/見開きは同じ仕組み。
   キー: S=見開き切替, D=綴じ方向切替。**設定は defparameter (*page-direction* /
   *spread* / *first-page-single*) として切り出し済み — 将来の設定 UI / 設定ファイルの土台。**
5. Fit モード (幅/高/全体) + ホイールズーム + ドラッグパン
6. 本棚 (フォルダツリー) + サムネイル + 履歴 + ブックマーク + 設定 UI
7. (拡張) RAR/7z = SharpCompress, PDF = Docnet, AVIF 等 = 追加 codec (NuGet)

## macOS .app 化の調査 (2026-06-19, 未解決の壁あり)

### 解決済み
- **未指定起動で窓が出ない**: `*default-image*` に Windows 固定パス
  (`C:/Windows/System32/@WLOGO_96x96.png`) をフォールバックにしていたため、
  macOS で open-book が例外 → 初期化中断。→ 廃止し未指定は空の本を開く (push 済)。
- **RenderTimer -6661 (Avalonia #14927, OPEN)**: CVDisplayLink ベースの RenderTimer が
  初期化失敗。ディスプレイ非接続/蓋閉じ/起動経路で再現。公開 API に RenderTimer 差替えは
  無いので `AvaloniaNativePlatformOptions.RenderingMode = [Software]` を macOS のみ適用
  (`Paalam.MacRenderFix`)。これで「ディスプレイ非接続な手元 exec」では起動継続を確認。
- **codesign 失敗 (contrib の bin/obj 誤認)**: DotCL.Runtime の contrib ソースに
  `bin/Release/net10.0` `obj/...` が紛れ込み、codesign が「ネスト bundle」と誤認して
  署名が壊れる。実行に不要 (中身は .deps.json/.pdb のみ) なので make-app.sh で除去。
- **`%maybe-autoclose` の暴発防止**: DOTCL_AUTOCLOSE_MS を *debug* かつ正の ms のときだけ
  有効化 (本番起動で環境のゴミ変数により即終了しないように)。
- **log-err / 起動 trace の出力先**: 署名済み .app の BaseDirectory は書けないので
  `~/paalam-error.log` / `~/paalam-trace.log` (ホーム) に出すよう変更。

### 未解決 (要・追加調査 / web 検索)
**Finder ダブルクリックでだけ即死する。** 一貫した観測:
- `dotnet run` / `open paalam.app` / `launchctl asuser <uid> open -W paalam.app`
  (= GUI セッション経由) → **すべて起動成功**。RUN-PAALAM 到達、正常終了まで確認。
- **Finder ダブルクリック → .NET の Main にすら到達せず SIGABRT (終了コード 33536=131)**。
  trace ログが 1 行も増えない = CLR 起動前/起動中に abort。
- システムログに AMFI / code signature / Library Validation / dyld のエラーは出ない。
  runningboard は running-active まで認識した直後に termination (0,0,33536)。
- 試して効かなかった: Software レンダリング / ad-hoc 署名し直し / JIT entitlements
  (allow-jit, allow-unsigned-executable-memory, disable-library-validation) /
  lsregister 再登録 / 右クリック→開く。
- 候補だが未確認: Finder が渡す `-psn_` 引数 (ただし launchctl asuser open では
  argc=1 で来ず、trace 未到達の説明にもならない)。LaunchServices の別経路。
- 関連 issue: AvaloniaUI/Avalonia #14927 (RenderTimer), #8983 (packed app crash)。
- 当面の実用上の回避: **`open paalam.app` または `dotnet run` で起動する** (Finder 不可)。

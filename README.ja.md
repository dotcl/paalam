# paalam

*[English](README.md) / 日本語*

**[dotcl](https://github.com/dotcl/dotcl)（.NET 上の Common Lisp）と [Avalonia](https://avaloniaui.net/) で書いた、NeeView 風の画像/コミック/PDF ビューア。**

*「paalam」はタガログ語で「さようなら」。*

UI もアプリケーションロジックも、ほぼすべて Common Lisp（`src/` 配下、機能ごとに分割）で書かれています。C# 側（`Program.cs` 一枚）が担うのは起動（boot）と一部の interop だけ。Windows / macOS / Linux で同じコードが動くクロスプラットフォーム GUI です。

> このプロジェクトは **dotcl で実用的な GUI アプリがどこまで書けるか** を示す技術デモを兼ねています。「Common Lisp で Avalonia の `Window` を組み立て、CLOS でドメインモデルを表現し、Lisp のスレッドで先読みする」——そういう構成の実例として読めます。

## 何が珍しいか

- **GUI を Common Lisp で記述** — `Avalonia.Application` 派生クラスすら Lisp の `dotnet:define-class` で emit している（`Paalam.App`）。`Window` / `Grid` / `Image` の組み立て、イベント結線、Dispatcher への post まで Lisp 側。
- **C# は薄い境界だけ** — `Program.cs`（1ファイル）は Avalonia アセンブリを先読みし、コンパイル済み Lisp を manifest からロードして `RUN-PAALAM` を呼ぶ。残るのはマネージドライブラリに任せた方が楽な interop だけ: PDFium のラスタライズ（`PdfDoc`、PDFtoImage/SkiaSharp）、EXIF 抽出（`Exif`、MetadataExtractor）、ネイティブのファイルダイアログ（`DialogHelper`）、macOS の描画修正（`MacRenderFix`）。なお PDF のテキスト/メタ/アウトライン抽出はかつての C# シム `Paalam.PdfText`（P/Invoke）から **Lisp の FFI（`src/pdf-ffi.lisp`、`dotnet:ffi`）へ移行済み**——raw なネイティブ呼び出しも Lisp 側で書ける。
- **ビルド時に Lisp を fasl へ事前コンパイル** — `DotCL.Runtime` の project-core build（`<DotclProjectAsd>`）が、ビルド時に ASDF system（`src/paalam.asd`）を in-process で fasl にコンパイルし、依存 fasl + manifest を出力に置く。実行時はソース非同梱・`compile-file` なしで一発ロード。
- **book プロトコル** — ページの供給源（ディレクトリ / zip / rar・7z・tar / PDF）を CLOS のプロトコルで抽象化（`directory-book` / `zip-book` / `sharp-book` / `pdf-book`）。GUI とは独立。

## 機能

- 見開き / 単ページ表示、右開き（漫画）/ 左開きの綴じ方向切替
- フィットモード（全体 / 幅 / 高）+ ホイールでカーソル中心ズーム、パン
- 対応形式: 画像フォルダ（HEIC/HEIF も対応、[SkiaSharp.Heic](https://www.nuget.org/packages/SkiaSharp.Heic) でデコード）、zip/cbz、rar/cbr・7z/cb7・tar/cbt（[SharpCompress](https://github.com/adamhathcock/sharpcompress)）、PDF（[PDFtoImage](https://github.com/sungaila/PDFtoImage) / PDFium）
- 先読みローダー — 裏のワーカースレッドがデコード済み Bitmap をキャッシュ。NAS 上の書庫でも快適にめくれる
- ページサムネ本棚 / フォルダツリー本棚 / ファイル情報・EXIF のサイドペイン
- 履歴（本ごとに最後のページを記憶して再開）+ しおり（ブックマーク）
- PDF 全文検索（`/`）とヒットのページ内ハイライト
- 素材クリップ — 集めたページを zip に掃き出し
- ドラッグ&ドロップで本を開く
- コマンドパレット（`:`）、キーバインドのカスタマイズ（設定ウィンドウ）

## ビルドと起動

必要なもの: **.NET 10 SDK**。

```sh
make build                      # または: dotnet build
make run                        # または: dotnet run
make run ARGS=/path/to/manga.zip   # フォルダ / 書庫 / 画像 / PDF を渡す
```

`dotnet run -- /path/to/manga.zip` のように直接渡すこともできます。
`PAALAM_PATH` 環境変数でも対象を指定できます。

## キーバインド（既定）

| キー | 動作 |
|---|---|
| `←` / `→` | 読み進む / 読み戻る（綴じ方向に従う） |
| `Space` `J` `PageDown` / `K` `PageUp` | 次の見開き / 前の見開き |
| `Home` / `End` | 先頭 / 末尾 |
| `[count]g` `gg` / `G` | ページ移動 / 先頭 / 末尾 |
| `S` / `D` | 見開き切替 / 綴じ方向切替 |
| `W` / `0` | フィットモード切替 / フィットへリセット |
| `F` `F11` | 全画面 |
| `R` | 再帰収集の切替 |
| `T` / `L` / `P` / `I` | サムネ本棚 / フォルダ本棚 / 素材ペイン / 情報パネル |
| `A` / `E` | 素材に追加 / zip に掃き出し |
| `B` / `[` / `]` | しおり切替 / 前のしおり / 次のしおり |
| `/` | 検索（PDF 全文 / ファイル名）。`n` / `N` でヒット巡回 |
| `:` | コマンドパレット |
| `Esc` | 戻る / 終了 |

キーは設定ウィンドウで変更でき、`keymap.lisp` に永続化されます。

## アーキテクチャ

```
Program.cs (C#)                  src/*.lisp (Common Lisp)
───────────────                  ─────────────────────────
Main()                           (defun run-paalam ...)
  Avalonia アセンブリ先読み    →    AppBuilder.Configure<Paalam.App>()
  DotclHost.LoadFromManifest()       .UsePlatformDetect()
  DotclHost.Call("RUN-PAALAM")       .StartWithClassicDesktopLifetime()
                                 (dotnet:define-class "Paalam.App" ...)
PdfDoc    PDFium ラスタライズ      OnFrameworkInitializationCompleted
Exif      EXIF (MetadataExtractor)  → build-main-window → Window
DialogHelper / MacRenderFix      book プロトコル (CLOS)
                                 pdf-ffi  PDFium テキスト/メタ FFI (dotnet:ffi)
                                 先読みローダー (dotcl-thread)
```

ビルド時に `src/paalam.asd` が fasl へ事前コンパイルされ、`bin/.../dotcl-fasl/` に
依存 fasl + `dotcl.core` + manifest（`dotcl-deps.txt`）が出力されます。実行時は
`Program.cs` が manifest を一括ロードするだけ（ソース非同梱）。

## 依存

NuGet 参照のみ（dotcl repo へのソース依存なし）:

- `DotCL.Runtime` — Common Lisp ランタイム + project-core build
- `Avalonia` / `Avalonia.Desktop` / `Avalonia.Themes.Fluent`
- `SharpCompress`（書庫）/ `PDFtoImage`（PDF）/ `SkiaSharp.Heic`（HEIC/HEIF）
- `MetadataExtractor`（EXIF / メタデータ、情報パネル）

## Windows インストーラ

```powershell
# Inno Setup 6 が必要: winget install --id JRSoftware.InnoSetup
make win-installer                                  # x64 + arm64 両方
powershell -File packaging\windows\build.ps1 -Arch x64   # 片方だけ
```

`dist\paalam-setup-<arch>.exe` が生成されます。**per-user インストール**
（`%LOCALAPPDATA%\Programs\paalam`、管理者権限不要）で、スタートメニューの
ショートカットとアンインストーラが付きます。中身は self-contained publish の
出力なので、.NET ランタイムが入っていないマシンでも起動できます。

ファイル関連付けはインストーラでは行いません（アプリ内の per-user 関連付けに
任せる方針）。スクリプト本体は `packaging/windows/`（`paalam.iss` + `build.ps1`）。

## macOS の .app バンドル

```sh
make app        # dist/paalam.app を生成
open dist/paalam.app
```

framework-dependent なので起動には .NET 10 ランタイムが必要です。`.app` は
署名していません（自分のマシンで使うぶんには起動できます）。他者へ配布する場合は
Developer ID 署名 + 公証が別途必要で、受け取った側では quarantine 属性の解除
（`xattr -dr com.apple.quarantine paalam.app`）が要ることがあります。

バンドル構築のロジックは `packaging/macos/`（`make-app.sh` + `Info.plist.in`）に
あります。

## Linux（AppImage / tar.gz）

```sh
make appimage   # dist/paalam-<version>-<arch>.AppImage を生成（self-contained）
make tarball    # dist/paalam-<version>-<arch>.tar.gz を生成（install.sh 付き）
```

どちらも self-contained（.NET ランタイム同梱）なので、対象マシンにランタイムは
不要です。tarball には XDG 準拠の `install.sh` / `uninstall.sh` が入っていて、
`~/.local/` 配下にインストールします。RID（`linux-x64` / `linux-arm64`）はホストから
自動検出されます。スクリプトは `packaging/linux/` にあります。

## ライセンス

[MIT](LICENSE) © SANO Masatoshi

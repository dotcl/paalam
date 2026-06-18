# paalam

**[dotcl](https://github.com/dotcl/dotcl)（.NET 上の Common Lisp）と [Avalonia](https://avaloniaui.net/) で書いた、NeeView 風の画像/コミック/PDF ビューア。**

UI もアプリケーションロジックも、ほぼすべて Common Lisp（`src/main.lisp`）で書かれています。C# 側（`Program.cs`）が担うのは起動（boot）とネイティブ interop だけ。Windows / macOS / Linux で同じコードが動くクロスプラットフォーム GUI です。

> このプロジェクトは **dotcl で実用的な GUI アプリがどこまで書けるか** を示す技術デモを兼ねています。「Common Lisp で Avalonia の `Window` を組み立て、CLOS でドメインモデルを表現し、Lisp のスレッドで先読みする」——そういう構成の実例として読めます。

## 何が珍しいか

- **GUI を Common Lisp で記述** — `Avalonia.Application` 派生クラスすら Lisp の `dotnet:define-class` で emit している（`Paalam.App`）。`Window` / `Grid` / `Image` の組み立て、イベント結線、Dispatcher への post まで Lisp 側。
- **C# は薄い境界だけ** — `Program.cs` は Avalonia アセンブリを先読みし、コンパイル済み Lisp を manifest からロードして `RUN-PAALAM` を呼ぶだけ。PDFium の raw P/Invoke など「Lisp の FFI で書けないネイティブ interop」のみ C# に置く。
- **ビルド時に Lisp を精コンパイル** — `DotCL.Runtime` の project-core build（`<DotclProjectAsd>`）が、ビルド時に ASDF system（`src/paalam.asd`）を in-process で fasl に精コンパイルし、依存 fasl + manifest を出力に置く。実行時はソース非同梱・`compile-file` なしで一発ロード。
- **book プロトコル** — ページの供給源（ディレクトリ / zip / rar・7z・tar / PDF）を CLOS のプロトコルで抽象化（`directory-book` / `zip-book` / `sharp-book` / `pdf-book`）。GUI とは独立。

## 機能

- 見開き / 単ページ表示、右開き（漫画）/ 左開きの綴じ方向切替
- フィットモード（全体 / 幅 / 高）+ ホイールでカーソル中心ズーム、パン
- 対応形式: 画像フォルダ、zip/cbz、rar/cbr・7z/cb7・tar/cbt（[SharpCompress](https://github.com/adamhathcock/sharpcompress)）、PDF（[PDFtoImage](https://github.com/sungaila/PDFtoImage) / PDFium）
- 先読みローダー — 裏のワーカースレッドがデコード済み Bitmap をキャッシュ。NAS 上の書庫でも快適にめくれる
- ページサムネ本棚 / フォルダツリー本棚のサイドペイン
- 履歴（本ごとに最後のページを記憶して再開）+ しおり（ブックマーク）
- PDF 全文検索（`/`）とヒットのページ内ハイライト
- 素材クリップ — 集めたページを zip に掃き出し
- ドラッグ&ドロップで本を開く
- コマンドパレット（`:`）、キーバインドのカスタマイズ（設定ウィンドウ）

## ビルドと起動

必要なもの: **.NET 10 SDK**。

```sh
dotnet build
dotnet run                      # 引数なし → 空のウィンドウ
dotnet run -- /path/to/manga.zip   # フォルダ / 書庫 / 画像 / PDF を渡す
```

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
| `T` / `L` / `P` | サムネ本棚 / フォルダ本棚 / 素材ペイン |
| `A` / `E` | 素材に追加 / zip に掃き出し |
| `B` / `[` / `]` | しおり切替 / 前のしおり / 次のしおり |
| `/` | 検索（PDF 全文 / ファイル名）。`n` / `N` でヒット巡回 |
| `:` | コマンドパレット |
| `Esc` / `Q` | 戻る / 終了 |

キーは設定ウィンドウで変更でき、`keymap.lisp` に永続化されます。

## アーキテクチャ

```
Program.cs (C#)                 src/main.lisp (Common Lisp)
─────────────                   ──────────────────────────
Main()                          (defun run-paalam ...)
  Avalonia アセンブリ先読み   →   AppBuilder.Configure<Paalam.App>()
  DotclHost.LoadFromManifest()      .UsePlatformDetect()
  DotclHost.Call("RUN-PAALAM")      .StartWithClassicDesktopLifetime()
                                 (dotnet:define-class "Paalam.App" ...)
PdfDoc / PdfText                   OnFrameworkInitializationCompleted
  PDFium ラスタライズ                → build-main-window → Window
  FPDFText_* の P/Invoke         book プロトコル (CLOS)
DragDropHelper / DialogHelper      先読みローダー (dotcl-thread)
```

ビルド時に `src/paalam.asd` が fasl へ精コンパイルされ、`bin/.../dotcl-fasl/` に
依存 fasl + `dotcl.core` + manifest（`dotcl-deps.txt`）が出力されます。実行時は
`Program.cs` が manifest を一括ロードするだけ（ソース非同梱）。

## 依存

NuGet 参照のみ（dotcl repo へのソース依存なし）:

- `DotCL.Runtime` — Common Lisp ランタイム + project-core build
- `Avalonia` / `Avalonia.Desktop` / `Avalonia.Themes.Fluent`
- `SharpCompress`（書庫）/ `PDFtoImage`（PDF）

## macOS について

framework-dependent ビルドなので、起動には .NET 10 ランタイムが必要です。
`.app` バンドルや署名付き配布は現時点では用意していません（自分のマシンで使うなら
`dotnet run` で十分動きます）。署名なしの `.app` を他者に渡す場合は、受け取った側で
quarantine 属性の解除（`xattr -dr com.apple.quarantine paalam.app`）が必要になります。

## ライセンス

[MIT](LICENSE) © SANO Masatoshi

#!/usr/bin/env bash
# dist/paalam.app を組み立てる (macOS, framework-dependent)。
# 通常は `make app` 経由で呼ばれる (publish 済みの出力を PUBLISH_DIR で受け取る)。
#
# 入力 (環境変数):
#   PUBLISH_DIR  framework-dependent publish の出力ディレクトリ (apphost を含む)
#   RID          ランタイム識別子 (osx-arm64 / osx-x64; 表示用)
#
# 出力: dist/paalam.app  (起動には .NET 10 ランタイムが必要)

set -euo pipefail
cd "$(dirname "$0")/../.."   # リポジトリルートへ

APP_NAME="paalam"
BUNDLE_ID="com.snmsts.paalam"
# Single source of truth = src/paalam.asd :version (no hand-synced literal).
VERSION="$(sed -n 's/.*:version[[:space:]]*"\([^"]*\)".*/\1/p' src/paalam.asd | head -1)"
: "${VERSION:?could not read :version from src/paalam.asd}"
DIST="dist"
APP="$DIST/$APP_NAME.app"
CONTENTS="$APP/Contents"
PUBLISH_DIR="${PUBLISH_DIR:-$DIST/publish-osx-arm64}"
RID="${RID:-osx-arm64}"

if [ ! -x "$PUBLISH_DIR/$APP_NAME" ]; then
  echo "!! apphost ($PUBLISH_DIR/$APP_NAME) が見つからない。先に 'make publish' を実行。" >&2
  exit 1
fi

echo "==> .app ツリーを構築 ($RID)"
rm -rf "$APP"
mkdir -p "$CONTENTS/MacOS" "$CONTENTS/Resources"
# publish 出力 (apphost / dll / dylib / dotcl-fasl / contrib / app.ico) を MacOS/ に置く。
# 実行ファイルからの相対 (AppContext.BaseDirectory) で dotcl-fasl/ や contrib/ を引くため
# 同一階層にまとめて入れる必要がある。
cp -R "$PUBLISH_DIR/." "$CONTENTS/MacOS/"

# dotcl の contrib ソースに紛れ込むビルド中間物 (bin/ obj/) を除去する。
# これらは実行に不要 (.deps.json/.pdb のみで dll は無い)。かつ bin/Release/net10.0 の
# ようなディレクトリが残ると codesign が「ネストした bundle」と誤認して署名が壊れ、
# Finder/Gatekeeper がアプリの起動自体を拒否する (open では通るが Finder で即死)。
find "$CONTENTS/MacOS/contrib" -type d \( -name bin -o -name obj \) -prune -exec rm -rf {} + 2>/dev/null || true

echo "==> アイコン (app.ico -> AppIcon.icns)"
ICONSET="$DIST/AppIcon.iconset"
rm -rf "$ICONSET"; mkdir -p "$ICONSET"
sips -s format png app.ico --out "$DIST/icon-src.png" >/dev/null
for sz in 16 32 64 128 256 512; do
  sips -z "$sz" "$sz"   "$DIST/icon-src.png" --out "$ICONSET/icon_${sz}x${sz}.png"    >/dev/null
  dbl=$((sz*2))
  sips -z "$dbl" "$dbl" "$DIST/icon-src.png" --out "$ICONSET/icon_${sz}x${sz}@2x.png" >/dev/null
done
iconutil -c icns "$ICONSET" -o "$CONTENTS/Resources/AppIcon.icns"
rm -rf "$ICONSET" "$DIST/icon-src.png"

echo "==> Info.plist"
sed -e "s/@APP_NAME@/$APP_NAME/g" \
    -e "s/@BUNDLE_ID@/$BUNDLE_ID/g" \
    -e "s/@VERSION@/$VERSION/g" \
    packaging/macos/Info.plist.in > "$CONTENTS/Info.plist"

# ad-hoc 署名: publish は apphost に apphost 名義の ad-hoc 署名を付けるが、その後
# MacOS/ にファイルを足すとバンドル署名が不整合になり、Finder/Gatekeeper が起動を
# 拒否する ("code has no resources but signature indicates they must be present")。
# open では通るが Finder ダブルクリックで即死する。バンドル全体を ad-hoc で署名し直す。
# 上で contrib の bin/obj を除いてあるので --deep のネスト誤認は起きない。
echo "==> ad-hoc 署名 (バンドル全体を署名し直し)"
codesign --remove-signature "$CONTENTS/MacOS/$APP_NAME" 2>/dev/null || true
codesign --force --deep --sign - "$APP"
codesign --verify --verbose "$APP" 2>&1 | tail -2 || true

echo
echo "完成: $APP"
echo "起動:  open $APP    (または Finder でダブルクリック)"
echo "注意:  framework-dependent なので .NET 10 ランタイムが必要。"
echo "       他マシンへコピーした場合は quarantine 解除が要ることがある:"
echo "         xattr -dr com.apple.quarantine $APP"

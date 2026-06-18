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
VERSION="0.1"
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

# 署名について:
#   .NET の publish は Contents/MacOS/ 直下に dll/json/dylib/contrib をすべて置く
#   非標準レイアウトのため、codesign が内部を別 bundle と誤認して通りにくい。
#   ad-hoc 署名なしでも自分の Mac では起動できる (検証済み)。他者配布時は
#   Developer ID 署名 + 公証が別途必要。
echo "==> (署名はスキップ: 自分の Mac で使う前提)"

echo
echo "完成: $APP"
echo "起動:  open $APP    (または Finder でダブルクリック)"
echo "注意:  framework-dependent なので .NET 10 ランタイムが必要。"
echo "       他マシンへコピーした場合は quarantine 解除が要ることがある:"
echo "         xattr -dr com.apple.quarantine $APP"

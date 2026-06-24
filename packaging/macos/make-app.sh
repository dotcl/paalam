#!/usr/bin/env bash
# Assemble dist/paalam.app (macOS, framework-dependent).
# Normally invoked via `make app` (receives the published output via PUBLISH_DIR).
#
# Input (environment variables):
#   PUBLISH_DIR  output dir of the framework-dependent publish (contains the apphost)
#   RID          runtime identifier (osx-arm64 / osx-x64; for display)
#
# Output: dist/paalam.app  (needs the .NET 10 runtime to launch)

set -euo pipefail
cd "$(dirname "$0")/../.."   # to the repo root

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
  echo "!! apphost ($PUBLISH_DIR/$APP_NAME) not found. Run 'make publish' first." >&2
  exit 1
fi

echo "==> Building the .app tree ($RID)"
rm -rf "$APP"
mkdir -p "$CONTENTS/MacOS" "$CONTENTS/Resources"
# Place the publish output (apphost / dll / dylib / dotcl-fasl / contrib / app.ico) under MacOS/.
# dotcl-fasl/ and contrib/ are resolved relative to the executable (AppContext.BaseDirectory),
# so they must all sit in the same directory.
cp -R "$PUBLISH_DIR/." "$CONTENTS/MacOS/"

# Remove build intermediates (bin/ obj/) that sneak into dotcl's contrib sources.
# They are unneeded at runtime (only .deps.json/.pdb, no dll). Also, a leftover directory like
# bin/Release/net10.0 makes codesign mistake it for a "nested bundle", breaking the signature, so
# Finder/Gatekeeper refuses to even launch the app (open works, but Finder dies instantly).
find "$CONTENTS/MacOS/contrib" -type d \( -name bin -o -name obj \) -prune -exec rm -rf {} + 2>/dev/null || true

echo "==> Icon (app.ico -> AppIcon.icns)"
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

# ad-hoc signing: publish gives the apphost an ad-hoc signature under the apphost's name, but
# adding files to MacOS/ afterwards makes the bundle signature inconsistent, so Finder/Gatekeeper
# refuses to launch ("code has no resources but signature indicates they must be present").
# open works, but a Finder double-click dies instantly. Re-sign the whole bundle ad-hoc.
# We removed contrib's bin/obj above, so --deep won't mistake them for nested bundles.
echo "==> ad-hoc signing (re-sign the whole bundle)"
codesign --remove-signature "$CONTENTS/MacOS/$APP_NAME" 2>/dev/null || true
codesign --force --deep --sign - "$APP"
codesign --verify --verbose "$APP" 2>&1 | tail -2 || true

echo
echo "Done: $APP"
echo "Launch: open $APP    (or double-click in Finder)"
echo "Note:   framework-dependent, so the .NET 10 runtime is required."
echo "        If copied to another machine, you may need to clear quarantine:"
echo "          xattr -dr com.apple.quarantine $APP"

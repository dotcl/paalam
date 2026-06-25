#!/usr/bin/env bash
# Assemble dist/paalam-<arch>.AppImage (Linux, self-contained).
# Normally invoked via `make appimage` (receives the published output via PUBLISH_DIR).
#
# Input (environment variables):
#   PUBLISH_DIR  output dir of the self-contained publish
#   RID          runtime identifier (linux-x64 / linux-arm64)
#
# Output: dist/paalam-<version>-<arch>.AppImage

set -euo pipefail
cd "$(dirname "$0")/../.."   # to the repo root

APP_NAME="paalam"
VERSION="$(sed -n 's/.*:version[[:space:]]*"\([^"]*\)".*/\1/p' src/paalam.asd | head -1)"
: "${VERSION:?could not read :version from src/paalam.asd}"
PUBLISH_DIR="${PUBLISH_DIR:-dist/publish-linux-x64}"
RID="${RID:-linux-x64}"
DIST="dist"
APPDIR="$DIST/AppDir"

case "$RID" in
  linux-x64)   ARCH="x86_64" ;;
  linux-arm64) ARCH="aarch64" ;;
  *) echo "Unknown RID: $RID" >&2; exit 1 ;;
esac

OUTPUT="$DIST/${APP_NAME}-${VERSION}-${ARCH}.AppImage"

if [ ! -x "$PUBLISH_DIR/$APP_NAME" ]; then
  echo "!! apphost ($PUBLISH_DIR/$APP_NAME) not found. Run 'make publish SELF_CONTAINED=true' first." >&2
  exit 1
fi

# Cache appimagetool under dist/; download from GitHub if missing.
APPIMAGETOOL="$DIST/appimagetool-${ARCH}.AppImage"
if [ ! -x "$APPIMAGETOOL" ]; then
  echo "==> Fetching appimagetool"
  mkdir -p "$DIST"
  case "$ARCH" in
    x86_64)   URL="https://github.com/AppImage/appimagetool/releases/download/continuous/appimagetool-x86_64.AppImage" ;;
    aarch64)  URL="https://github.com/AppImage/appimagetool/releases/download/continuous/appimagetool-aarch64.AppImage" ;;
  esac
  curl -fsSL -o "$APPIMAGETOOL" "$URL"
  chmod +x "$APPIMAGETOOL"
fi

echo "==> Building AppDir ($RID)"
rm -rf "$APPDIR"
mkdir -p "$APPDIR"
cp -R "$PUBLISH_DIR/." "$APPDIR/"

echo "==> Icon (app.ico -> paalam.png)"
python3 - <<'PYEOF'
from PIL import Image

ico = Image.open("app.ico")
best = max(ico.ico.sizes(), key=lambda s: s[0])
ico.size = best
img = ico.convert("RGBA").resize((256, 256), Image.LANCZOS)
img.save("dist/AppDir/paalam.png")
print("icon: 256x256 PNG saved")
PYEOF

echo "==> .desktop file"
cat > "$APPDIR/paalam.desktop" <<EOF
[Desktop Entry]
Type=Application
Name=paalam
Exec=paalam %f
Icon=paalam
Comment=Manga / image viewer
Categories=Graphics;Viewer;
MimeType=image/jpeg;image/png;image/webp;image/avif;image/heic;image/heif;application/zip;application/x-cbz;application/x-cbr;application/pdf;
StartupNotify=true
EOF

echo "==> AppRun"
cat > "$APPDIR/AppRun" <<'EOF'
#!/usr/bin/env bash
HERE="$(dirname "$(readlink -f "${0}")")"
# Direct .NET's bundle-extract temp dir so it doesn't litter /tmp.
export DOTNET_BUNDLE_EXTRACT_BASE_DIR="${HOME}/.cache/paalam"
exec "$HERE/paalam" "$@"
EOF
chmod +x "$APPDIR/AppRun"

echo "==> Packaging AppImage ($OUTPUT)"
# --appimage-extract-and-run avoids requiring FUSE on the build host.
ARCH="$ARCH" "$APPIMAGETOOL" --appimage-extract-and-run "$APPDIR" "$OUTPUT"

echo
echo "Done: $OUTPUT"
echo "Run:  ./$OUTPUT"
echo "      or ./$OUTPUT /path/to/manga.zip"

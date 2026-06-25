#!/usr/bin/env bash
# Assemble dist/paalam-<version>-<arch>.tar.gz (Linux, self-contained).
# After extracting, run install.sh to install under ~/.local/ following XDG conventions.
#
# Input (environment variables):
#   PUBLISH_DIR  output dir of the self-contained publish
#   RID          runtime identifier (linux-x64 / linux-arm64)
#
# Output: dist/paalam-<version>-<arch>.tar.gz

set -euo pipefail
cd "$(dirname "$0")/../.."   # to the repo root

APP_NAME="paalam"
VERSION="$(sed -n 's/.*:version[[:space:]]*"\([^"]*\)".*/\1/p' src/paalam.asd | head -1)"
: "${VERSION:?could not read :version from src/paalam.asd}"
PUBLISH_DIR="${PUBLISH_DIR:-dist/publish-linux-x64}"
RID="${RID:-linux-x64}"
DIST="dist"

case "$RID" in
  linux-x64)   ARCH="x86_64" ;;
  linux-arm64) ARCH="aarch64" ;;
  *) echo "Unknown RID: $RID" >&2; exit 1 ;;
esac

STAGE="$DIST/tarball-${ARCH}"
OUTPUT="$DIST/${APP_NAME}-${VERSION}-${ARCH}.tar.gz"

if [ ! -x "$PUBLISH_DIR/$APP_NAME" ]; then
  echo "!! apphost ($PUBLISH_DIR/$APP_NAME) not found. Run 'make publish SELF_CONTAINED=true' first." >&2
  exit 1
fi

echo "==> Building staging directory ($RID)"
rm -rf "$STAGE"
mkdir -p "$STAGE/bin"
cp -R "$PUBLISH_DIR/." "$STAGE/bin/"

echo "==> Icon (app.ico -> $APP_NAME.png)"
STAGE="$STAGE" python3 - <<'PYEOF'
import os
from PIL import Image

ico = Image.open("app.ico")
best = max(ico.ico.sizes(), key=lambda s: s[0])
ico.size = best
img = ico.convert("RGBA").resize((256, 256), Image.LANCZOS)
img.save(os.environ["STAGE"] + "/paalam.png")
print("icon: 256x256 PNG saved")
PYEOF

echo "==> .desktop file"
cat > "$STAGE/$APP_NAME.desktop" <<EOF
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

echo "==> install.sh"
cat > "$STAGE/install.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
HERE="$(dirname "$(readlink -f "${0}")")"
APP="paalam"
SHARE="$HOME/.local/share/$APP"
BIN_DIR="$HOME/.local/bin"
APPS_DIR="$HOME/.local/share/applications"
ICONS_DIR="$HOME/.local/share/icons/hicolor/256x256/apps"

mkdir -p "$SHARE" "$BIN_DIR" "$APPS_DIR" "$ICONS_DIR"
cp -R "$HERE/bin/." "$SHARE/"
ln -sf "$SHARE/$APP" "$BIN_DIR/$APP"
cp "$HERE/$APP.desktop" "$APPS_DIR/"
cp "$HERE/$APP.png" "$ICONS_DIR/"
update-desktop-database "$APPS_DIR" 2>/dev/null || true

echo "Installed"
echo "  binary : $BIN_DIR/$APP"
echo "  data   : $SHARE"
echo "  If ~/.local/bin is not in PATH, add it to your shell config."
EOF
chmod +x "$STAGE/install.sh"

echo "==> uninstall.sh"
cat > "$STAGE/uninstall.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
APP="paalam"
rm -rf "$HOME/.local/share/$APP"
rm -f  "$HOME/.local/bin/$APP"
rm -f  "$HOME/.local/share/applications/$APP.desktop"
rm -f  "$HOME/.local/share/icons/hicolor/256x256/apps/$APP.png"
update-desktop-database "$HOME/.local/share/applications" 2>/dev/null || true
echo "Uninstalled"
EOF
chmod +x "$STAGE/uninstall.sh"

echo "==> Packaging tar.gz ($OUTPUT)"
DIRNAME="${APP_NAME}-${VERSION}-${ARCH}"
(cd "$DIST" && mv "tarball-${ARCH}" "$DIRNAME" && tar czf "${APP_NAME}-${VERSION}-${ARCH}.tar.gz" "$DIRNAME" && mv "$DIRNAME" "tarball-${ARCH}")

echo
echo "Done: $OUTPUT"
echo "Usage:"
echo "  tar xzf $OUTPUT"
echo "  cd ${APP_NAME}-${VERSION}-${ARCH}"
echo "  ./install.sh"

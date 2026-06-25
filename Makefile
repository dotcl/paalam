# paalam — build / run / package
#
#   make build              debug build
#   make run                dotnet run (pass args via ARGS: make run ARGS=~/manga.zip)
#   make publish            framework-dependent publish (RID auto-detected)
#   make app                macOS: build dist/paalam.app (self-contained)
#   make appimage           Linux: build dist/paalam-<version>-<arch>.AppImage (self-contained)
#   make tarball            Linux: build dist/paalam-<version>-<arch>.tar.gz (with install.sh)
#   make win-installer      Windows: build dist/paalam-setup-<arch>.exe (Inno Setup)
#   make clean              remove bin/ obj/ dist/
#
# RID is auto-detected from the host. To set it explicitly: make publish RID=linux-x64

CONFIG     ?= Release
UNAME_S    := $(shell uname -s)
UNAME_M    := $(shell uname -m)

# Pick osx-arm64 / osx-x64 / linux-x64 / linux-arm64 from uname.
ifeq ($(UNAME_S),Darwin)
  ifeq ($(UNAME_M),arm64)
    RID ?= osx-arm64
  else
    RID ?= osx-x64
  endif
else ifeq ($(UNAME_S),Linux)
  ifeq ($(UNAME_M),aarch64)
    RID ?= linux-arm64
  else
    RID ?= linux-x64
  endif
endif

PUBLISH_DIR := dist/publish-$(RID)

.PHONY: build run publish app appimage tarball win-installer clean

build:
	dotnet build -c $(CONFIG)

run:
	dotnet run -c $(CONFIG) -- $(ARGS)

# publish. SELF_CONTAINED=true bundles the .NET runtime.
# Default is framework-dependent (lighter). `make app` / `make appimage` use self-contained:
# launching via a Finder double-click does not inherit shell env vars like DOTNET_ROOT, so a
# framework-dependent build (especially with dotnet in a non-standard path like Homebrew) can
# fail to find the runtime and SIGABRT before reaching Main. Bundling makes startup env-independent.
SELF_CONTAINED ?= false
publish:
	rm -rf $(PUBLISH_DIR)
	dotnet publish -c $(CONFIG) -r $(RID) --self-contained $(SELF_CONTAINED) -o $(PUBLISH_DIR)

# macOS .app bundle. Publish self-contained (runtime bundled) and delegate.
app:
ifneq ($(UNAME_S),Darwin)
	@echo "make app is macOS-only (current: $(UNAME_S)). Use make appimage on Linux."
	@exit 1
endif
	$(MAKE) publish SELF_CONTAINED=true
	PUBLISH_DIR="$(PUBLISH_DIR)" RID="$(RID)" packaging/macos/make-app.sh

# Linux AppImage. Publish self-contained (runtime bundled) and delegate.
appimage:
ifneq ($(UNAME_S),Linux)
	@echo "make appimage is Linux-only (current: $(UNAME_S)). Use make app on macOS."
	@exit 1
endif
	$(MAKE) publish SELF_CONTAINED=true
	PUBLISH_DIR="$(PUBLISH_DIR)" RID="$(RID)" packaging/linux/make-appimage.sh

# Linux tar.gz with XDG-compliant install.sh / uninstall.sh.
tarball:
ifneq ($(UNAME_S),Linux)
	@echo "make tarball is Linux-only (current: $(UNAME_S))."
	@exit 1
endif
	$(MAKE) publish SELF_CONTAINED=true
	PUBLISH_DIR="$(PUBLISH_DIR)" RID="$(RID)" packaging/linux/make-tarball.sh

# Windows installer (Inno Setup). Windows-only.
# self-contained publish -> packaging/windows/build.ps1 runs ISCC for both RIDs.
win-installer:
	powershell -ExecutionPolicy Bypass -File packaging/windows/build.ps1 $(if $(ARCH),-Arch $(ARCH))

clean:
	rm -rf bin obj dist

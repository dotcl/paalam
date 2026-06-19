# paalam — build / run / package
#
#   make build              デバッグビルド
#   make run                dotnet run (引数は ARGS で渡す: make run ARGS=~/manga.zip)
#   make publish            framework-dependent publish (RID 自動判定)
#   make app                macOS: dist/paalam.app を作る (framework-dependent)
#   make clean              bin/ obj/ dist/ を消す
#
# RID はホストから自動判定。明示するなら: make publish RID=linux-x64

CONFIG     ?= Release
UNAME_S    := $(shell uname -s)
UNAME_M    := $(shell uname -m)

# osx-arm64 / osx-x64 / linux-x64 / linux-arm64 を uname から決める。
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

.PHONY: build run publish app clean

build:
	dotnet build -c $(CONFIG)

run:
	dotnet run -c $(CONFIG) -- $(ARGS)

# publish。SELF_CONTAINED=true で .NET ランタイムを同梱する。
# 既定は framework-dependent (軽い)。`make app` は self-contained を使う:
# Finder ダブルクリック起動は DOTNET_ROOT 等のシェル環境変数を引き継がないため、
# framework-dependent だと (特に Homebrew など非標準パスの dotnet では) ランタイムを
# 発見できず Main 到達前に SIGABRT する。同梱すれば環境非依存で起動できる。
SELF_CONTAINED ?= false
publish:
	rm -rf $(PUBLISH_DIR)
	dotnet publish -c $(CONFIG) -r $(RID) --self-contained $(SELF_CONTAINED) -o $(PUBLISH_DIR)

# macOS の .app バンドル。ランタイム同梱 (self-contained) で publish して委譲。
app:
ifneq ($(UNAME_S),Darwin)
	@echo "make app は macOS 専用です (現在: $(UNAME_S))。Linux の配布は別途 (packaging/linux/)。"
	@exit 1
endif
	$(MAKE) publish SELF_CONTAINED=true
	PUBLISH_DIR="$(PUBLISH_DIR)" RID="$(RID)" packaging/macos/make-app.sh

clean:
	rm -rf bin obj dist

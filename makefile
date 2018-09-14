# (C) 2017 David Lettier
# lettier.com

.RECIPEPREFIX != ps

_NAME="movie-monad"
_VERSION="0.0.7.0"
_STACK=stack --allow-different-user
_STACK_SNAPSHOT_INSTALL_ROOT=`$(_STACK) path --snapshot-install-root`
_STACK_SNAPSHOT_INSTALL_ROOT_BIN="$(_STACK_SNAPSHOT_INSTALL_ROOT)/bin"
_STACK_PATH_LOCAL_BIN=`$(_STACK) path --local-bin`
_STACK_GHC_EXE=`$(_STACK) path --compiler-exe`
_STACK_GHC_BIN=`$(_STACK) path --compiler-bin`
_STACK_PATHS=$(_STACK_PATH_LOCAL_BIN):$(_STACK_GHC_BIN):$(_STACK_SNAPSHOT_INSTALL_ROOT):$(_STACK_SNAPSHOT_INSTALL_ROOT_BIN)
_GHC_VERSION=`$(_STACK) ghc -- --version | sed 's|The Glorious Glasgow Haskell Compilation System, version ||g'`
_CABAL=env PATH=$(PATH):$(_STACK_PATHS) "$(_STACK_SNAPSHOT_INSTALL_ROOT_BIN)/cabal"
_CABAL_SANDBOX_DIR=".cabal-sandbox"
_APPDATA_DIR="$(_CABAL_SANDBOX_DIR)/share/metainfo"
_APPLICATIONS_DESKTOP_DIR="$(_CABAL_SANDBOX_DIR)/share/applications"
_ICONS_HICOLOR_SCALABLE_APPS_DIR="$(_CABAL_SANDBOX_DIR)/share/icons/hicolor/scalable/apps"
_PACKAGING_LINUX_COMMON_DIR="./packaging/linux/common"
_LINUX_PACKAGE_DIR=$(_NAME)"-linux-"$(_VERSION)

export PATH := $(PATH):$(_STACK_PATHS)

all: cabal_install

setup:
  $(_STACK) update && \
  $(_STACK) setup && \
  $(_STACK) install alex happy && \
  $(_STACK) install haskell-gi

cabal_update: setup
  $(_CABAL) update

cabal_sandbox_init: cabal_update
  $(_CABAL) sandbox init

cabal_clean: cabal_update
  $(_CABAL) clean && \
  $(_CABAL) sandbox init && \
  $(_CABAL) sandbox delete && \
  $(_CABAL) sandbox init

cabal_install_dependencies: cabal_sandbox_init
  $(_CABAL) --require-sandbox install --dependencies-only --force-reinstalls -j -w $(_STACK_GHC_EXE)

cabal_configure: cabal_install_dependencies
  $(_CABAL) --require-sandbox configure --enable-relocatable -w $(_STACK_GHC_EXE)

cabal_build: cabal_configure
  $(_CABAL) --require-sandbox build -j

applications_desktop:
  mkdir -p $(_APPLICATIONS_DESKTOP_DIR) && \
  cp "$(_PACKAGING_LINUX_COMMON_DIR)/com.lettier.$(_NAME).desktop" $(_APPLICATIONS_DESKTOP_DIR)/

icons_hicolor_scalable_apps:
  mkdir -p $(_ICONS_HICOLOR_SCALABLE_APPS_DIR) && \
  cp "$(_PACKAGING_LINUX_COMMON_DIR)/com.lettier.$(_NAME).svg" $(_ICONS_HICOLOR_SCALABLE_APPS_DIR)/

appdata:
  mkdir -p $(_APPDATA_DIR) && \
  cp "$(_PACKAGING_LINUX_COMMON_DIR)/com.lettier.$(_NAME).appdata.xml" $(_APPDATA_DIR)/

cabal_install: cabal_build appdata applications_desktop icons_hicolor_scalable_apps
  $(_CABAL) --require-sandbox install --enable-relocatable --force-reinstalls -j -w $(_STACK_GHC_EXE)

package_cabal_sandbox_for_linux: cabal_install
  rm -rf "._movie_monad_trash_" && \
  mkdir -p "._movie_monad_trash_" && \
  mkdir -p $(_LINUX_PACKAGE_DIR) && \
  touch "$(_LINUX_PACKAGE_DIR).tar.gz" && \
  mv "$(_LINUX_PACKAGE_DIR).tar.gz" "._movie_monad_trash_/" && \
  mv $(_LINUX_PACKAGE_DIR) "._movie_monad_trash_" && \
  mkdir -p $(_LINUX_PACKAGE_DIR) && \
  cp -R "$(_CABAL_SANDBOX_DIR)/." $(_LINUX_PACKAGE_DIR) && \
  find "$(_LINUX_PACKAGE_DIR)/share/x86_64-linux-ghc-$(_GHC_VERSION)/" -mindepth 1 -maxdepth 1 -type d \
    -not -path '*movie-monad*' -exec mv {} "._movie_monad_trash_/" \; && \
  find "$(_LINUX_PACKAGE_DIR)/lib/x86_64-linux-ghc-$(_GHC_VERSION)/" -mindepth 1 -maxdepth 1 -type d \
    -exec mv {} "._movie_monad_trash_/" \; && \
  find "$(_LINUX_PACKAGE_DIR)/bin/" -type f -not -name '*movie-monad*' -exec mv {} "._movie_monad_trash_/" \; && \
  find "$(_LINUX_PACKAGE_DIR)/" -mindepth 1 -maxdepth 1 -type d -not -path '*bin*' -not -path '*lib*' -not -path '*share*' \
    -exec mv {} "._movie_monad_trash_/" \; && \
  find "$(_LINUX_PACKAGE_DIR)/" -mindepth 1 -maxdepth 1 -type f -not -path '*bin*' -not -path '*lib*' -not -path '*share*' \
    -exec mv {} "._movie_monad_trash_/" \; && \
  tar -zcvf "$(_LINUX_PACKAGE_DIR).tar.gz" $(_LINUX_PACKAGE_DIR)

build_sdist:
  $(_STACK) sdist

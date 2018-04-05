# (C) 2017 David Lettier
# lettier.com

.RECIPEPREFIX != ps

_NAME="movie-monad"
_STACK=stack --allow-different-user
_STACK_SNAPSHOT_INSTALL_ROOT=`$(_STACK) path --snapshot-install-root`
_STACK_SNAPSHOT_INSTALL_ROOT_BIN="$(_STACK_SNAPSHOT_INSTALL_ROOT)/bin"
_STACK_PATH_LOCAL_BIN=`$(_STACK) path --local-bin`
_STACK_GHC_EXE=`$(_STACK) path --compiler-exe`
_STACK_GHC_BIN=`$(_STACK) path --compiler-bin`
_STACK_PATHS=$(_STACK_PATH_LOCAL_BIN):$(_STACK_GHC_BIN):$(_STACK_SNAPSHOT_INSTALL_ROOT):$(_STACK_SNAPSHOT_INSTALL_ROOT_BIN)
_CABAL=env PATH=$(PATH):$(_STACK_PATHS) "$(_STACK_SNAPSHOT_INSTALL_ROOT_BIN)/cabal"
_CABAL_SANDBOX_DIR=".cabal-sandbox"
_APPLICATIONS_DESKTOP_DIR="$(_CABAL_SANDBOX_DIR)/share/applications"
_ICONS_HICOLOR_SCALABLE_APPS_DIR="$(_CABAL_SANDBOX_DIR)/share/icons/hicolor/scalable/apps"
_PACKAGING_LINUX_COMMON_DIR="./packaging/linux/common"

export PATH := $(PATH):$(_STACK_PATHS)

all: install

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

cabal_install: cabal_build
  $(_CABAL) --require-sandbox install --enable-relocatable --force-reinstalls -j -w $(_STACK_GHC_EXE)

applications_desktop:
  mkdir -p $(_APPLICATIONS_DESKTOP_DIR) && \
  cp $(_PACKAGING_LINUX_COMMON_DIR)/$(_NAME).desktop $(_APPLICATIONS_DESKTOP_DIR)/

icons_hicolor_scalable_apps:
  mkdir -p $(_ICONS_HICOLOR_SCALABLE_APPS_DIR) && \
  cp $(_PACKAGING_LINUX_COMMON_DIR)/$(_NAME)-icon.svg $(_ICONS_HICOLOR_SCALABLE_APPS_DIR)/

install: cabal_install applications_desktop icons_hicolor_scalable_apps

build_sdist:
  $(_STACK) sdist

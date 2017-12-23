# (C) 2017 David Lettier
# lettier.com

.RECIPEPREFIX != ps

STACK=stack --allow-different-user
STACK_SNAPSHOT_INSTALL_ROOT=`$(STACK) path --snapshot-install-root`
STACK_PATH_LOCAL_BIN=`$(STACK) path --local-bin`
STACK_GHC_EXE=`$(STACK) path --compiler-exe`
STACK_GHC_BIN=`$(STACK) path --compiler-bin`
STACK_PATHS=$(STACK_PATH_LOCAL_BIN):$(STACK_GHC_BIN):$(STACK_SNAPSHOT_INSTALL_ROOT)
CABAL=env PATH=$(PATH):$(STACK_PATHS) $(STACK_SNAPSHOT_INSTALL_ROOT)/bin/cabal

export PATH := $(PATH):$(STACK_PATH_LOCAL_BIN)

all: setup build

setup:
  $(STACK) setup && \
  $(STACK) update && \
  $(STACK) build alex && \
  $(STACK) build happy

build: setup
  $(STACK) build

install: build
  $(STACK) install

run: install
  $(STACK) exec -- movie-monad

build_sdist:
  $(STACK) sdist

cabal_update: setup
  $(STACK) build cabal-install && \
  $(CABAL) update

cabal_clean: cabal_update
  $(CABAL) clean && \
  $(CABAL) sandbox init && \
  $(CABAL) sandbox delete && \
  $(CABAL) sandbox init

cabal_install_relocatable_executable: cabal_clean
  $(CABAL) sandbox init && \
  $(CABAL) --require-sandbox install --dependencies-only --force-reinstalls -j -w $(STACK_GHC_EXE) && \
  $(CABAL) --require-sandbox configure --enable-relocatable -w $(STACK_GHC_EXE) && \
  $(CABAL) --require-sandbox build -j && \
  $(CABAL) --require-sandbox install --enable-relocatable --force-reinstalls -j -w $(STACK_GHC_EXE)

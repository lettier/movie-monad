# (C) 2017 David Lettier
# lettier.com

.RECIPEPREFIX != ps

STACK=stack --allow-different-user
STACK_PATH_LOCAL_BIN=`$(STACK) path --local-bin`
STACK_GHC_EXE=`$(STACK) path --compiler-exe`
STACK_GHC_BIN=`$(STACK) path --compiler-bin`
STACK_PATHS=$(STACK_PATH_LOCAL_BIN):$(STACK_GHC_BIN)
CABAL=env PATH=$(PATH):$(STACK_PATHS) $(STACK_PATH_LOCAL_BIN)/cabal

export PATH := $(PATH):$(STACK_PATH_LOCAL_BIN)

all: setup build

setup:
  $(STACK) setup && \
  $(STACK) update && \
  $(STACK) install cabal-install && \
  $(STACK) install alex && \
  $(STACK) install happy

build: setup
  $(STACK) build

install: build
  $(STACK) install

run: install
  $(STACK) exec -- movie-monad

build_sdist:
  $(STACK) sdist

cabal_update: setup
  $(CABAL) update

cabal_clean: cabal_update
  $(CABAL) clean && \
  $(CABAL) sandbox delete && \
  $(CABAL) sandbox init

cabal_install_relocatable_executable: cabal_clean
  $(CABAL) sandbox init && \
  $(CABAL) --require-sandbox install --dependencies-only -j -w $(STACK_GHC_EXE) && \
  $(CABAL) --require-sandbox configure --enable-relocatable -w $(STACK_GHC_EXE) && \
  $(CABAL) --require-sandbox build -j && \
  $(CABAL) --require-sandbox install --enable-relocatable -j -w $(STACK_GHC_EXE)

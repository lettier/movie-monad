# (C) 2017 David Lettier
# lettier.com

.RECIPEPREFIX != ps

CABAL=stack exec -- cabal exec -- cabal

all: setup update build

setup:
  stack setup

update: setup
  stack update

build: setup
  stack build

install: build
  stack  install

run: install
  stack exec -- movie-monad

build_dist:
  stack sdist

clean_cabal_sandbox: setup
  $(CABAL) sandbox delete && \
  $(CABAL) sandbox init

build_relocatable_executable:
  $(CABAL) sandbox init && \
  $(CABAL) --require-sandbox install --dependencies-only -j && \
  $(CABAL) --require-sandbox configure --enable-relocatable && \
  $(CABAL) --require-sandbox build -j && \
  $(CABAL) --require-sandbox install --enable-relocatable -j

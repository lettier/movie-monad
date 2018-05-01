#!/usr/bin/env bash

# (C) 2018 David Lettier
# lettier.com

_SCRIPT_DIR=`dirname "${BASH_SOURCE[0]}"`

while read _dependency; do
  _URL="https://hackage.haskell.org/package/$_dependency/$_dependency.tar.gz"
  wget $_URL -q -O "$_SCRIPT_DIR/$_dependency"
  _SHA=`sha256sum $_SCRIPT_DIR/$_dependency | sed 's/ .*//'`
  rm "$_SCRIPT_DIR/$_dependency"
  echo \
    "    {
      \"name\": \"$_dependency\",
      "only-arches": [
        "x86_64"
      ],
      \"buildsystem\": \"simple\",
      \"build-commands\": [
        \"ghc -threaded --make Setup\",
        \"./Setup configure --prefix=/app\",
        \"./Setup build\",
        \"./Setup install\"
      ],
      \"sources\": [
        {
          \"type\": \"archive\",
          \"sha256\": \"$_SHA\",
          \"url\": \"$_URL\"
        }
      ]
    },"
done < "$_SCRIPT_DIR/dependencies-list.txt"

#!/usr/bin/env bash

# (C) 2018 David Lettier
# lettier.com

_NAME="movie-monad"
_VER="0.0.6.0"
_SCRIPT_DIR=`dirname "${BASH_SOURCE[0]}"`
_APP_IMAGE_NAME="$_NAME-$_VER-x86_64.AppImage"
_APP_IMAGE_PATH="$_SCRIPT_DIR/$_APP_IMAGE_NAME"
_APP_IMAGE_TOOL="$_SCRIPT_DIR/../appimagetool-x86_64.AppImage"

$_APP_IMAGE_TOOL -vn "$_SCRIPT_DIR/$_NAME.AppDir" $_APP_IMAGE_PATH
chmod a+x $_APP_IMAGE_PATH
$_APP_IMAGE_PATH

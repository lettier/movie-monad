#!/usr/bin/env bash

# (C) 2017 David Lettier
# lettier.com

_MOVIE_MONAD_VERSION="0.0.5.0"
_MOVIE_MONAD_RELEASES_DOWNLOAD="https://github.com/lettier/movie-monad/releases/download/$_MOVIE_MONAD_VERSION"
_MOVIE_MONAD_PACKAGING_LINUX_COMMON="https://raw.githubusercontent.com/lettier/movie-monad/master/packaging/linux/common"
_MOVIE_MONAD_APP_IMAGE="movie-monad-$_MOVIE_MONAD_VERSION-x86_64.AppImage"

echo -e "Installing Movie Monad...\n"

cd "$HOME"

mkdir -p "$HOME/.local/bin" "$HOME/.local/share/applications" "$HOME/.icons"

cd "$HOME/.local/bin"
wget "$_MOVIE_MONAD_RELEASES_DOWNLOAD/$_MOVIE_MONAD_APP_IMAGE" -O "$_MOVIE_MONAD_APP_IMAGE"
chmod a+x "$_MOVIE_MONAD_APP_IMAGE"

cd "$HOME/.icons"
wget "$_MOVIE_MONAD_PACKAGING_LINUX_COMMON/movie-monad-icon.svg" -O "movie-monad-icon.svg"

cd "$HOME/.local/share/applications"
wget "$_MOVIE_MONAD_PACKAGING_LINUX_COMMON/movie-monad.desktop" -O "movie-monad.desktop"
echo -e "`sed '$ d' movie-monad.desktop`\nExec=$HOME/.local/bin/$_MOVIE_MONAD_APP_IMAGE" > "movie-monad.desktop"

cd "$HOME"
touch ".profile"
echo -e "\nexport PATH=\"\$PATH:\$HOME/.local/bin\"" >> ".profile"

echo -e "\nMovie Monad installed."

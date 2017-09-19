#!/usr/bin/env bash

# (C) 2017 David Lettier
# lettier.com

MOVIE_MONAD_VERSION="0.0.2.0"
MOVIE_MONAD_RELEASES_DOWNLOAD="https://github.com/lettier/movie-monad/releases/download/$MOVIE_MONAD_VERSION"
MOVIE_MONAD_PACKAGING_LINUX="https://raw.githubusercontent.com/lettier/movie-monad/master/packaging/linux"
MOVIE_MONAD_APP_IMAGE="movie-monad-$MOVIE_MONAD_VERSION-x86_64.AppImage"

echo -e "Installing Movie Monad...\n"
cd "$HOME"
mkdir -p "$HOME/.local/bin" "$HOME/.local/share/applications" "$HOME/.icons"
cd "$HOME/.local/bin"
wget "$MOVIE_MONAD_RELEASES_DOWNLOAD/$MOVIE_MONAD_APP_IMAGE" -O "$MOVIE_MONAD_APP_IMAGE"
chmod a+x "$MOVIE_MONAD_APP_IMAGE"
cd "$HOME/.icons"
wget "$MOVIE_MONAD_PACKAGING_LINUX/movie-monad.png" -O "movie-monad.png"
cd "$HOME/.local/share/applications"
wget "$MOVIE_MONAD_PACKAGING_LINUX/movie-monad.desktop" -O "movie-monad.desktop"
echo "Exec=$HOME/.local/bin/$MOVIE_MONAD_APP_IMAGE" >> "movie-monad.desktop"
cd "$HOME"
touch ".profile"
echo -e "\nexport PATH=\"\$PATH:\$HOME/.local/bin\"" >> ".profile"
echo -e "\nMovie Monad installed."

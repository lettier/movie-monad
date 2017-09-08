![Movie Monad](https://i.imgur.com/d7Hz6Cv.png)

# Movie Monad

A desktop video player built with Haskell that uses GStreamer and GTK+.

## Screenshots

![GUI showing Sintel from the Blender Foundation](https://i.imgur.com/SLse3s9.jpg)

## Documentation

[Let's make a GTK Video Player with Haskell](https://lettier.github.io/posts/2017-08-30-haskell-gtk-video-player.html)

## Install

### Hackage

```
# Install Cabal (https://www.haskell.org/platform/)
# Install Cabal Install (https://www.haskell.org/platform/)
# Install XQuartz (https://www.xquartz.org/) if using Mac OSX or macOS
# Install Xming X Server (https://sourceforge.net/projects/xming/) if using Microsoft Windows
# Install GTK+ 3.* (https://www.gtk.org/download/index.php)
# Install ExifTool (https://www.sno.phy.queensu.ca/~phil/exiftool/)
cabal install movie-monad
movie-monad
```

### GitHub

```bash
# Install Git (https://git-scm.com/downloads)
# Install XQuartz (https://www.xquartz.org/) if using Mac OSX or macOS
# Install Xming X Server (https://sourceforge.net/projects/xming/) if using Microsoft Windows
# Install GTK+ 3.* (https://www.gtk.org/download/index.php)
# Install Haskell (https://www.haskell.org/platform/)
# Install Haksell Stack (https://www.haskell.org/platform/)
# Install ExifTool (https://www.sno.phy.queensu.ca/~phil/exiftool/)
git clone https://github.com/lettier/movie-monad.git
cd movie-monad/
stack setup
stack install
stack exec -- movie-monad
# Or just `movie-monad` if `stack path | grep local-bin-path` is in your `echo $PATH`
```

## License

See [LICENSE](LICENSE).

## Copyright

(C) 2017 David Lettier  
[lettier.com](http://www.lettier.com/)

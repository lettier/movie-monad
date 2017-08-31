![Movie Monad](https://i.imgur.com/Vt9Bipy.png)

# Movie Monad

A desktop video player built with Haskell that uses GStreamer and GTK+.

## Screenshots

<img src="http://i.imgur.com/QOMXSwo.jpg" width="600" alt="GUI" title= "GUI" />

## Documentation

[Let's make a GTK Video Player with Haskell](https://lettier.github.io/posts/2017-08-30-haskell-gtk-video-player.html)

## Install

### GitHub

```bash
# Install Git
# Install Haskell
# Install Haksell Stack
# Install ExifTool
git clone https://github.com/lettier/movie-monad.git
cd movie-monad/
stack setup
stack install --dependencies-only
haskell-gi -o lib/gi-gdkx11/GdkX11.overrides -O lib/gi-gdkx11 GdkX11-3.0
haskell-gi -o lib/gi-xlib/xlib.overrides     -O lib/gi-xlib xlib
stack build
stack install
stack exec -- movie-monad
```

## License

See [LICENSE](LICENSE).

(C) 2017 David Lettier  
[lettier.com](http://www.lettier.com/)

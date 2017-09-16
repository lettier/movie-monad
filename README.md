![Movie Monad](https://i.imgur.com/gdsyIMv.png)

# Movie Monad

A desktop video player built with Haskell that uses GStreamer and GTK+.

## Screenshots

![GUI showing Sintel from the Blender Foundation](https://i.imgur.com/SLse3s9.jpg)

## Features

* Local or remote video file playback
    * `file://`
    * `http://`
    * `https://`
* Window size selection
* Fullscreen mode
* Seek
* Play and pause
* Volume
* Keyboard shortcuts
    * Mute toggle
        * `m`
        * `AudioMute`
    * Fullscreen toggle
        * `f`
    * Volume up
        * `Ctrl+ArrowUp` or `Command+ArrowUp` if using a Mac
        * `AudioRaiseVolume`
    * Volume down
        * `Ctrl+ArrowDown` or `Command+ArrowDown` if using a Mac
        * `AudioLowerVolume`
    * Play/pause toggle
        * `Space`
        * `AudioPlay`

## Documentation

[Let's make a GTK Video Player with Haskell](https://lettier.github.io/posts/2017-08-30-haskell-gtk-video-player.html)

## Dependencies

* [Haskell](https://www.haskell.org/platform/)
* X11
    * [Windows](https://sourceforge.net/projects/xming/)
    * [Mac](https://www.xquartz.org/)
    * Linux
* [GTK+](https://www.gtk.org/download/index.php)
* [GStreamer](https://gstreamer.freedesktop.org/download/)

## Install & Run

### Hackage

```bash
# Install Cabal (https://www.haskell.org/platform/)
# Install Cabal Install (https://www.haskell.org/platform/)
# Install XQuartz (https://www.xquartz.org/) if using Mac OSX or macOS
# Install Xming X Server (https://sourceforge.net/projects/xming/) if using Microsoft Windows
# Install GTK+ 3.* (https://www.gtk.org/download/index.php)
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

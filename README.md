![Movie Monad](https://i.imgur.com/gdsyIMv.png)

# Movie Monad

A desktop video player built with Haskell that uses GStreamer and GTK+.

## Screenshots

![GUI showing Sintel from the Blender Foundation](https://i.imgur.com/UBNYbER.jpg)

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
    * Show on-screen controls
        * `c`
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

### Runtime

* X11
    * [Windows](https://sourceforge.net/projects/xming/)
    * [Mac](https://www.xquartz.org/)
    * Linux
* [GTK+ >= 3.10](https://www.gtk.org/download/index.php)
* [GStreamer == 1.0](https://gstreamer.freedesktop.org/download/)

### Build

* [Haskell](https://www.haskell.org/platform/)

## Install & Run

### Linux

#### AppImage

```bash
# Install GTK+ >= 3.10 (https://www.gtk.org/download/index.php)
# Install GStreamer == 1.0 (https://gstreamer.freedesktop.org/download/)
# Install all of the GStreamer Plugins (https://en.wikipedia.org/wiki/GStreamer#Plug-ins)
# Visit https://github.com/lettier/movie-monad/releases
# * equals the current version
# Download the latest AppImage movie-monad-*-x86_64.AppImage
wget https://github.com/lettier/movie-monad/releases/download/*/movie-monad-*-x86_64.AppImage
chmod a+x movie-monad-*-x86_64.AppImage
./movie-monad-*-x86_64.AppImage
# If you would like to install the AppImage, you can run the following
cd
wget "https://raw.githubusercontent.com/lettier/movie-monad/master/packaging/linux/movie-monad-install.sh" -O "movie-monad-install.sh"
chmod +x "movie-monad-install.sh"
./movie-monad-install.sh
```

### Hackage

```bash
# Install XQuartz (https://www.xquartz.org/) if using Mac OSX or macOS
# Install Xming X Server (https://sourceforge.net/projects/xming/) if using Microsoft Windows
# Install GTK+ >= 3.10 (https://www.gtk.org/download/index.php)
# Install GStreamer == 1.0 (https://gstreamer.freedesktop.org/download/)
# Install all of the GStreamer Plugins (https://en.wikipedia.org/wiki/GStreamer#Plug-ins)
# Install Cabal (https://www.haskell.org/platform/)
# Install Cabal Install (https://www.haskell.org/platform/)
cabal install movie-monad
movie-monad
```

### GitHub

```bash
# Install XQuartz (https://www.xquartz.org/) if using Mac OSX or macOS
# Install Xming X Server (https://sourceforge.net/projects/xming/) if using Microsoft Windows
# Install GTK+ >= 3.10 (https://www.gtk.org/download/index.php)
# Install GStreamer == 1.0 (https://gstreamer.freedesktop.org/download/)
# Install all of the GStreamer Plugins (https://en.wikipedia.org/wiki/GStreamer#Plug-ins)
# Install Git (https://git-scm.com/downloads)
# Install GNU Make (https://www.gnu.org/software/make/)
# Install Haskell (https://www.haskell.org/platform/)
# Install Haksell Stack (https://www.haskell.org/platform/)
git clone https://github.com/lettier/movie-monad.git
cd movie-monad/
make
make install
make run
```

## License

See [LICENSE](LICENSE).

## Copyright

(C) 2017 David Lettier  
[lettier.com](http://www.lettier.com/)

![Movie Monad](https://i.imgur.com/gdsyIMv.png)

# Movie Monad

A desktop video player built with Haskell that uses GStreamer and GTK+.

## Screenshots

![GUI showing Sintel from the Blender Foundation](https://i.imgur.com/UBNYbER.jpg)

![GUI showing Big Buck Bunny from the Blender Foundation](https://i.imgur.com/Tgmk7SW.png)

![GUI showing the About window as seen on the Mac](https://i.imgur.com/hiix2Wm.png)

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

## Keyboard Shortcuts

* Mute/Unmute
    * `m`
    * `AudioMute`
* Fullscreen/Windowed
    * `f`
* Show Controls
    * `c`
* Volume Up
    * `ArrowUp`
    * `AudioRaiseVolume`
* Volume Down
    * `ArrowDown`
    * `AudioLowerVolume`
* Play/Pause
    * `Space`
    * `AudioPlay`

## Tested On

### Linux

* [Ubuntu 16.04](https://www.ubuntu.com/desktop)
* [Ubuntu 14.04](https://www.ubuntu.com/desktop)
* [Linux Mint 18.2](https://linuxmint.com/)
* [Manjaro 17.04](https://manjaro.org/)
* [Deepin 15.4.1](https://www.deepin.org/en/dde/)

### Mac

* [macOS Sierra 10.12.6](https://en.wikipedia.org/wiki/MacOS_Sierra)

## Dependencies

### Run

* [GTK+ >= 3.10](https://www.gtk.org/download/index.php)
* [GStreamer >= 1.0](https://gstreamer.freedesktop.org/download/)
    * [GStreamer Good Plug-ins](https://gstreamer.freedesktop.org/modules/gst-plugins-good.html)
    * [GStreamer Bad Plug-ins](https://gstreamer.freedesktop.org/modules/gst-plugins-bad.html)

### Build

* [Haskell](https://www.haskell.org/platform/)

## Install & Run

### Linux

#### AppImage

```bash
# Install GTK+ >= 3.10 (https://www.gtk.org/download/index.php)
# Install GStreamer >= 1.0 (https://gstreamer.freedesktop.org/download/)
# Install GStreamer Good Plug-ins (https://gstreamer.freedesktop.org/modules/gst-plugins-good.html)
# Install GStreamer Bad Plug-ins >= 1.8 (https://gstreamer.freedesktop.org/modules/gst-plugins-bad.html)
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

### Mac

```bash
# Install Homebrew (https://brew.sh/#install)
cd ~/Downloads
brew update && brew upgrade
brew install git
git clone https://github.com/lettier/movie-monad.git
cd movie-monad
brew cask install haskell-platform
brew install pkg-config libffi libav libsvg librsvg libogg libvorbis openh264 theora \
  gobject-introspection cairo gdk-pixbuf gsettings-desktop-schemas gtk+3 gtk-mac-integration \
  gstreamer gst-libav gst-plugins-base gst-plugins-good
brew install --with-gtk+3 gst-plugins-bad
stack setup
stack install hsc2hs
stack install
export PATH=$PATH:"$HOME/.local/bin/"
movie-monad
```

### Hackage

```bash
# Install GTK+ >= 3.10 (https://www.gtk.org/download/index.php)
# Install GStreamer >= 1.0 (https://gstreamer.freedesktop.org/download/)
# Install GStreamer Good Plug-ins (https://gstreamer.freedesktop.org/modules/gst-plugins-good.html)
# Install GStreamer Bad Plug-ins >= 1.8 (https://gstreamer.freedesktop.org/modules/gst-plugins-bad.html)
# Install Cabal (https://www.haskell.org/platform/)
# Install Cabal Install (https://www.haskell.org/platform/)
cabal install movie-monad
movie-monad
```

### GitHub

```bash
# Install GTK+ >= 3.10 (https://www.gtk.org/download/index.php)
# Install GStreamer >= 1.0 (https://gstreamer.freedesktop.org/download/)
# Install GStreamer Good Plug-ins (https://gstreamer.freedesktop.org/modules/gst-plugins-good.html)
# Install GStreamer Bad Plug-ins >= 1.8 (https://gstreamer.freedesktop.org/modules/gst-plugins-bad.html)
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

## Developer Documentation

[Let's make a GTK Video Player with Haskell](https://lettier.github.io/posts/2017-08-30-haskell-gtk-video-player.html)

## License

See [LICENSE](LICENSE).

## Copyright

(C) 2017 David Lettier  
[lettier.com](http://www.lettier.com/)

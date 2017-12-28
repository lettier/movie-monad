![Movie Monad](https://i.imgur.com/gdsyIMv.png)

# Movie Monad

A free and simple to use video player built with Haskell.

## Screenshots

### Linux

![GUI showing "Caminandes 3: LLamigos" from the Blender Foundation](https://i.imgur.com/aYf6XOB.png)

![GUI showing "Sintel" from the Blender Foundation](https://i.imgur.com/UBNYbER.jpg)

### Mac

![GUI showing "Big Buck Bunny" from the Blender Foundation](https://i.imgur.com/Tgmk7SW.png)

![GUI showing the About window as seen on the Mac](https://i.imgur.com/pTJLQdM.png)

## Features

* Local or Remote Video Playback
    * `file://`
    * `http://`
    * `https://`
* Window Size Quick Select
* Responsive Window Resizing
* Fullscreen Mode
* Seek
* Play and Pause
* Volume
* Command-line Play
    * `movie-monad ./path/to/video/file.mp4`
    * `movie-monad http://www.domain.tld/path/to/video/file.mp4`

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
* Seek Left
    * `ArrowLeft`
* Seek Right
    * `ArrowRight`
* Play/Pause
    * `Space`
    * `AudioPlay`

## Tested On

### Linux

* [Ubuntu 17.10](https://www.ubuntu.com/desktop)
* [Ubuntu 16.04](https://www.ubuntu.com/desktop)
* [Ubuntu 14.04](https://www.ubuntu.com/desktop)
* [Linux Mint 18.2](https://linuxmint.com/)
* [Deepin 15.4.1](https://www.deepin.org/en/dde/)
* [Manjaro 17.04](https://manjaro.org/)
* [Antergos 17.10](https://antergos.com/)
* [Fedora 27](https://getfedora.org/)
* [Solus 3](https://solus-project.com/2017/08/15/solus-3-released/)

### Mac

* [macOS Sierra 10.12.6](https://en.wikipedia.org/wiki/MacOS_Sierra)

## Dependencies

### Run

* [GTK+ >= 3.10](https://www.gtk.org/download/index.php)
* [GStreamer >= 1.0](https://gstreamer.freedesktop.org/download/)
    * [GStreamer Good Plug-ins](https://gstreamer.freedesktop.org/modules/gst-plugins-good.html)
    * [GStreamer Bad Plug-ins >= 1.8](https://gstreamer.freedesktop.org/modules/gst-plugins-bad.html)

### Build

* [Haskell](https://www.haskell.org/platform/prior.html)
    * GHC == 8.0.2

## Install & Run

### Linux

#### AppImage

##### Download & Run

```bash
# Install GTK+ >= 3.10 (https://www.gtk.org/download/index.php)
# Install GStreamer >= 1.0 (https://gstreamer.freedesktop.org/download/)
# Install GStreamer Good Plug-ins (https://gstreamer.freedesktop.org/modules/gst-plugins-good.html)
# Install GStreamer Bad Plug-ins >= 1.8 (https://gstreamer.freedesktop.org/modules/gst-plugins-bad.html)
# Install Wget (https://www.gnu.org/software/wget/)
# Visit https://github.com/lettier/movie-monad/releases
# Download the latest AppImage movie-monad-0.0.4.0-x86_64.AppImage
wget https://github.com/lettier/movie-monad/releases/download/0.0.4.0/movie-monad-0.0.4.0-x86_64.AppImage
chmod a+x movie-monad-0.0.4.0-x86_64.AppImage
./movie-monad-0.0.4.0-x86_64.AppImage
```

##### Install

```bash
# Install GTK+ >= 3.10 (https://www.gtk.org/download/index.php)
# Install GStreamer >= 1.0 (https://gstreamer.freedesktop.org/download/)
# Install GStreamer Good Plug-ins (https://gstreamer.freedesktop.org/modules/gst-plugins-good.html)
# Install GStreamer Bad Plug-ins >= 1.8 (https://gstreamer.freedesktop.org/modules/gst-plugins-bad.html)
# Install Wget (https://www.gnu.org/software/wget/)
wget "https://raw.githubusercontent.com/lettier/movie-monad/master/scripts/sh/install-app-image.sh" -O "movie-monad-install-app-image.sh"
chmod +x "movie-monad-install-app-image.sh"
./movie-monad-install-app-image.sh
```

#### [Arch/Manjaro/Antergos](https://aur.archlinux.org/packages/movie-monad/)

```bash
cd
# Install Git (https://git-scm.com/downloads)
sudo pacman -S git
# Install Movie Monad from the AUR (https://aur.archlinux.org/packages/movie-monad/)
mkdir -p movie-monad-tmp
cd movie-monad-tmp
git clone https://aur.archlinux.org/movie-monad.git
cd movie-monad
makepkg -sic
cd
rm -rf movie-monad-tmp
movie-monad
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

### [Hackage](https://hackage.haskell.org/package/movie-monad)

```bash
# Install GObject Introspection (https://wiki.gnome.org/action/show/Projects/GObjectIntrospection)
# Install GTK+ >= 3.10 (https://www.gtk.org/download/index.php)
# Install GStreamer >= 1.0 (https://gstreamer.freedesktop.org/download/)
# Install GStreamer Good Plug-ins (https://gstreamer.freedesktop.org/modules/gst-plugins-good.html)
# Install GStreamer Bad Plug-ins >= 1.8 (https://gstreamer.freedesktop.org/modules/gst-plugins-bad.html)
# Install Cabal, GHC == 8.0.2 (https://www.haskell.org/platform/prior.html)
# Install Cabal Install (https://www.haskell.org/platform/prior.html)
cabal install movie-monad
movie-monad
```

### [GitHub](https://github.com/lettier/movie-monad)

```bash
# Install GObject Introspection (https://wiki.gnome.org/action/show/Projects/GObjectIntrospection)
# Install GTK+ >= 3.10 (https://www.gtk.org/download/index.php)
# Install GStreamer >= 1.0 (https://gstreamer.freedesktop.org/download/)
# Install GStreamer Good Plug-ins (https://gstreamer.freedesktop.org/modules/gst-plugins-good.html)
# Install GStreamer Bad Plug-ins >= 1.8 (https://gstreamer.freedesktop.org/modules/gst-plugins-bad.html)
# Install Git (https://git-scm.com/downloads)
# Install GNU Make (https://www.gnu.org/software/make/)
# Install Haskell, GHC == 8.0.2 (https://www.haskell.org/platform/prior.html)
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

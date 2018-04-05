![Movie Monad](https://i.imgur.com/gdsyIMv.png)

# Movie Monad? Another video player? Really?

Yes really.  

Movie Monad is a free and simple to use video player made with Haskell.
Originally it was a proof of concept to add video playback to
[Gifcurry](https://lettier.github.io/gifcurry/) (another great app—check it out).
Nowadays it's a lightweight yet mighty media player used all over the
[world](https://snapcraft.io/movie-monad#js-snap-map)
everyday.

Movie Monad is cross platform so you'll never have to compute without it.
You can play files on your computer or stream videos from the web.
Play, pause, seek, repeat, resize, expand, and turn it up—Movie Monad gets out of
the way so you can watch the videos you love.

So put down your copy of mpv, VLC, or Elmedia and try out Movie Monad—it's FREE!

## What does Movie Monad look like?

![Movie Monad](https://i.imgur.com/0zjy4xz.gif)
![Movie Monad](https://i.imgur.com/0SvqTpp.gif)

## What can I do with Movie Monad?

* You can play video files from your hard drive or stream videos from the web.
    * To play web videos, click the file open button, paste the URL into the text box, and click open.
    * Movie Monad can handle `file://`, `https://`, and `http://`.
* You can play and pause.
* You can seek/scrub through the video.
* You can put the video on repeat.
* You can turn subtitles off or on and pick your language.
* You can turn up, turn down, or mute the volume.
* You can expand the video to fullscreen.
* You can quickly resize the video to standard sizes.
* You can resize the video to a custom size by resizing the window.
    * Movie Monad will responsively adjust the video size to fill the window.
* You can play videos from the command line.
    * `movie-monad ./path/to/video/file.mp4`
    * `movie-monad http://www.domain.tld/path/to/video/file.mp4`

## What are the keyboard controls?

* Mute/Unmute
    * `m`
    * `AudioMute`
* Fullscreen/Windowed
    * `f`
* Show Controls
    * `c`
* Toggle Repeat
    * `r`
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

## How do I get a copy of Movie Monad?

Movie Monad works on Linux, Mac, and most likely Windows.
Make sure you have GStreamer and GTK+ installed on your machine.
To find the latest version of Movie Monad, head over to the
[releases page](https://github.com/lettier/movie-monad/releases).

### I use Linux.

If you use Linux then the easiest way to grab a copy of Movie Monad is by downloading the
[AppImage](https://github.com/lettier/movie-monad/releases/download/0.0.5.0/movie-monad-0.0.5.0-x86_64.AppImage).
After you download the
[AppImage](https://github.com/lettier/movie-monad/releases/download/0.0.5.0/movie-monad-0.0.5.0-x86_64.AppImage),
right click on it, select permissions, and check the box near execute.
With that out of the way—you're all set—just double click on the AppImage
and Movie Monad will pop up.

You can also download and install the
[AppImage](https://github.com/lettier/movie-monad/releases/download/0.0.5.0/movie-monad-0.0.5.0-x86_64.AppImage)
using the handy
[AppImage install script](https://raw.githubusercontent.com/lettier/movie-monad/master/packaging/linux/app-image/movie-monad-install-app-image-script.sh)
(right click the link and select "Save link as...").
Download the script, right click on it, select permissions, check the box near execute, and double click on it.
You should now see Movie Monad listed alongside your other installed programs.

#### I use Arch/Manjaro/Antergos/pacman.

If you'd rather install Movie Monad via `pacman` then copy the following into your terminal.

```bash
cd "$HOME/Downloads"
sudo pacman -S git
mkdir -p build-movie-monad
cd build-movie-monad
git clone https://aur.archlinux.org/movie-monad.git
cd movie-monad
makepkg -sic
cd "$HOME/Downloads"
rm -rf build-movie-monad
cd
movie-monad
```

#### I use Ubuntu/Mint/Debian/Deepin/snap.

Movie Monad is available as a snap from [Snapcraft](https://snapcraft.io/).
If you don't already have `snap`, go ahead and install it using the command `sudo apt install snapd`.

You can install the
[Movie Monad snap](https://snapcraft.io/movie-monad)
right from your browser or via the command line.
For the command line route, paste the following into your terminal.

```bash
snap install movie-monad
sudo snap connect movie-monad:mount-observe
sudo snap connect movie-monad:removable-media
sudo snap connect movie-monad:raw-usb
movie-monad
```

### I use Mac.

Mac users can download and run the
[Mac install script](https://raw.githubusercontent.com/lettier/movie-monad/master/packaging/mac/movie-monad-mac-install-script.command)
(hold down control, click the link, and select "Save Link As...").
After running the install script, a shortcut to Movie Monad will be on your desktop.

### I'm a Haskell developer.

If you develop Haskell programs then the easiest way to build Movie Monad is with
[Haskell Stack](https://docs.haskellstack.org/en/stable/README/).
Copy the following into your terminal.

```bash
cd "$HOME/Downloads"
git clone https://github.com/lettier/movie-monad.git
cd movie-monad
stack update
stack setup
stack install alex happy
stack install haskell-gi
stack install
stack exec -- movie-monad
```

## What dependencies does Movie Monad use?

### To run Movie Monad.

* [GTK+ >= 3.10](https://www.gtk.org/download/index.php)
* [GStreamer >= 1.0](https://gstreamer.freedesktop.org/download/)
    * [GStreamer Good Plug-ins](https://gstreamer.freedesktop.org/modules/gst-plugins-good.html)
    * [GStreamer Bad Plug-ins >= 1.8](https://gstreamer.freedesktop.org/modules/gst-plugins-bad.html)

### To build Movie Monad.

* [GObject Introspection](https://wiki.gnome.org/action/show/Projects/GObjectIntrospection)
* [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

## What is the license?

See [LICENSE](LICENSE).

## Who wrote Movie Monad?

(C) 2017 David Lettier  
[lettier.com](http://www.lettier.com/)

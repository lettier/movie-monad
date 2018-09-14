![Movie Monad](https://i.imgur.com/gdsyIMv.png)

## Is Movie Monad the free video player I've been looking for?

Yes. :heavy_check_mark:  

Movie Monad is a free and simple to use video player made with Haskell.
Originally it was a proof of concept to add video playback to
[Gifcurry](https://lettier.github.io/gifcurry/) (another great app—check it out).
Nowadays it's a lightweight yet mighty media player used all over the
[world](https://snapcraft.io/movie-monad#js-snap-map)
:globe_with_meridians: everyday.

Movie Monad is cross platform so you'll never have to watch without it.
Stream videos from the web or play the files stored on your computer.
Play, pause, seek, repeat, resize, expand, and turn it up—Movie Monad gets out of
the way so you can watch the videos you love. :heartbeat:

So try out Movie Monad—it'll make your life simpler. :sunglasses:

## What does Movie Monad look like? :eyes:

![Movie Monad](https://i.imgur.com/r1btBMF.gif)

## What can I do with Movie Monad? :thinking:

* You can play video files from your hard drive :computer: or stream videos from the web. :spider_web:
    * To play web videos, click the file open button, paste the URL into the text box, and click open.
    * Movie Monad can handle `file://`, `https://`, and `http://`.
* You can play and pause. :play_or_pause_button:
* You can seek/scrub :rewind: :fast_forward: through the video. :vhs:
* You can put the video on repeat. :repeat:
* You can slow down :snail: or speed up :racehorse: the video (great for lectures).
* You can turn subtitles off or on and pick your language.
* You can turn up :arrow_up:, turn down :arrow_down:, or mute :mute: the volume. :sound:
* You can expand the video to full screen.
* You can quickly resize the video to standard sizes.
* You can resize the video to a custom size by resizing the window.
    * Movie Monad will responsively adjust the video size to fill the window. :boom:
* You can play videos from the command line. :cl:
    * `movie-monad ./path/to/video/file.webm`
    * `movie-monad http://www.domain.tld/path/to/video/file.webm`

## What are the keyboard controls? :keyboard:

* Mute/Unmute
    * `m`
    * `AudioMute`
* Full Screen/Windowed
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

## How do I get a copy of Movie Monad? :floppy_disk:

Movie Monad works on Linux, Mac, and most likely Windows.
Make sure you have GStreamer and GTK+ installed on your machine.
To find the latest version of Movie Monad, head over to the
[releases page](https://github.com/lettier/movie-monad/releases).

### I use Linux. :penguin:

If you use Linux then the easiest way to grab a copy of Movie Monad is by downloading the
[AppImage](https://github.com/lettier/movie-monad/releases/download/0.0.7.0/movie-monad-0.0.7.0-x86_64.AppImage).
After you download the
[AppImage](https://github.com/lettier/movie-monad/releases/download/0.0.7.0/movie-monad-0.0.7.0-x86_64.AppImage),
right click on it, select permissions, and check the box near execute.
With that out of the way—you're all set—just double click on the AppImage
and Movie Monad will pop up.

You can also download and install the
[AppImage](https://github.com/lettier/movie-monad/releases/download/0.0.7.0/movie-monad-0.0.7.0-x86_64.AppImage)
using the handy
[AppImage install script](https://raw.githubusercontent.com/lettier/movie-monad/master/packaging/linux/app-image/movie-monad-install-app-image-script.sh)
(right click the link and select "Save link as...").
Download the script, right click on it, select permissions, check the box near execute, and double click on it.
You should now see Movie Monad listed alongside your other installed programs.

#### I use Arch, Manjaro, Antergos, or pacman. :ghost:

You can install Movie Monad via `pacman`. Copy the following into your terminal.

```bash
cd "$HOME/Downloads"
sudo pacman -S git gstreamer gst-plugins-base-libs gst-plugins-base gst-plugins-good gst-plugins-bad gst-libav
git clone https://aur.archlinux.org/movie-monad.git
cd movie-monad
makepkg -sic
cd "$HOME/Downloads"
rm -rf movie-monad
cd
movie-monad
```

#### I use Fedora, SUSE, CentOS, Red Hat, Flatpak, Flathub, or GNOME Software. :tophat:

You can install Movie Monad as a Flatpak via [Flathub](https://flathub.org/apps/details/com.lettier.movie-monad).

```bash
flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
flatpak install flathub com.lettier.movie-monad
```

You can also find it in GNOME Software. Just search for Movie Monad.

#### I use Ubuntu, Mint, Debian, Deepin, or Snap. :cyclone:

Movie Monad is available as a snap from [Snapcraft](https://snapcraft.io/).
If you don't already have `snap`, go ahead and install it using the command `sudo apt install snapd`.

You can install the
[Movie Monad snap](https://snapcraft.io/movie-monad)
right from your browser or via the command line.
For the command line route, paste the following into your terminal.

```bash
snap install movie-monad
sudo snap connect movie-monad:removable-media
movie-monad
```

### I use Mac. :apple:

Mac users can download the
[Mac install script](https://raw.githubusercontent.com/lettier/movie-monad/master/packaging/mac/movie-monad-mac-install-script.command)
by holding down control, clicking the link,
selecting "Save Link As...", selecting where Downloads, and clicking save.
To run the script, hold down the command key and press the space bar. Now type `terminal` and hit enter.
After the terminal comes up, copy and paste the following.

```bash
cd ~/Downloads
chmod +x movie-monad-mac-install-script.command
./movie-monad-mac-install-script.command
```

Once it's finished, a shortcut to Movie Monad will be on your desktop.

### I'm a Haskell developer. :desktop_computer:

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
    * [GStreamer Base Plug-ins](https://gstreamer.freedesktop.org/modules/gst-plugins-base.html)
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

# [Movie Monad](https://github.com/lettier/movie-monad)

## CHANGELOG

-------------------------------------------------------------------------------

### 0.0.6.0

#### Added

- Flatpak packaging
- Fade out animation to bottom controls box
- Variable rate playback

#### Changed

- Fixed memory error due to resetting playbin after releasing its reference
- Fixed minimum window height for very small videos
- Fixed file dialog text entry pop up style issue
- Replace seek bar with buffer spinner when buffering
- Move about dialog button to top button controls

#### Removed

-

-------------------------------------------------------------------------------

### 0.0.5.0

#### Added

- Repeat mode
- Custom icons for new bottom controls
- Custom styling for new bottom controls
- Added a one second fade in animation for the bottom controls
- Added extra error checking around window width combo box selection
- Mac install script

#### Changed

- Tooltips
- Use a SVG icon instead of PNG
- Fixed video info being changed before selecting a video
- Use play and pause image widget names to determine state of the play pause button
- If the user sets the window to a custom size, select nothing in the window selection combo box
- Set timeout for hiding the bottom controls to seven seconds instead of five
- AppImage meta data
- AppImage install script
- Snap packaging
- AUR packaging

#### Removed

-

-------------------------------------------------------------------------------

### 0.0.4.0

#### Added

- Subtitle support
- C src directory
- C FFI files

#### Changed

- Clamped keyboard shortcut seek left and right to 0.0 and 100.0
- Reduced keyboard shortcut seek left and right proportion

#### Removed

-

-------------------------------------------------------------------------------

### 0.0.3.0

#### Added

- PKGBUILD for Arch Linux
- AppImage specific desktop file
- Linux screenshot
- Ubuntu snapcraft.yaml
- Seek left and right keyboard shortcuts

#### Changed

- Cabal file
    - Added stack.yaml
    - Added makefile
    - Changed base to use ghc-8.0.1 or ghc-8.0.2
- README
- makefile
    - Ensure cabal binary is stack version
    - Force reinstalls for cabal install
- Dependencies
- Allow the window to be freely resized no matter the current width selection
- Adjusts the window height so that the video can fill the width of the window

#### Removed

- Version from mac screenshot

-------------------------------------------------------------------------------

### 0.0.2.0

#### Added

- Hides the cursor and the on-screen controls when the mouse is idle
- New Logo
- URL support
- Keyboard shortcuts
- makefile
- Icon
- AppImage
- AppImage installation script
- Support for GTK 3.10
- macOS screenshots
- macOS build instructions
- Screensaver and power management enable/disable
- Video file path command line option that, if present, plays the video on window realized

#### Changed

- Cabal file
    - Corrected source-repository
- Logo
- README
- Dependencies
- Icon
- Play/Pause toggle to button
- ICO file to PNG
- Switched from GstVideoOverlay to GtkSink

#### Removed

- ExifTool requirement
- Old logo PNG file
- X11 dependency

-------------------------------------------------------------------------------

### 0.0.1.0

#### Added

- Fullscreen mode

#### Changed

- Screenshot
- Main function
    - Extracted callback functions into their own functions
- Dependencies to match Hackage

#### Removed

- `lib` directory

-------------------------------------------------------------------------------

### 0.0.0.0

#### Added

- File Loader
- Seek
- Pause
- Play
- Window Width Selector
- About
- Screenshot
- Logo
- README
- LICENSE
- Documentation

#### Changed

-

#### Removed

-

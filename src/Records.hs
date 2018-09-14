{-
  Movie Monad
  (C) 2017 David Lettier
  lettier.com
-}

module Records where

import Data.IORef
import Data.Text
import qualified GI.Gtk
import qualified GI.Gst

data Application =
  Application
    { guiObjects :: GuiObjects
    , ioRefs     :: IORefs
    , playbin    :: GI.Gst.Element
    , playbinBus :: GI.Gst.Bus
    }

data GuiObjects =
  GuiObjects
    { window                           :: GI.Gtk.Window
    , fileChooserDialogButtonLabel     :: GI.Gtk.Label
    , fileChooserDialogLabel           :: GI.Gtk.Label
    , infoDialogLabel                  :: GI.Gtk.Label
    , fileChooserDialog                :: GI.Gtk.Dialog
    , videoLocationEntry               :: GI.Gtk.Entry
    , fileChooserWidget                :: GI.Gtk.FileChooserWidget
    , videoWidget                      :: GI.Gtk.Widget
    , topControlsBox                   :: GI.Gtk.Box
    , bottomControlsBox                :: GI.Gtk.Box
    , seekScale                        :: GI.Gtk.Scale
    , fileChooserDialogButton          :: GI.Gtk.Button
    , playPauseButton                  :: GI.Gtk.Button
    , fullScreenButton                 :: GI.Gtk.Button
    , fileChooserDialogCancelButton    :: GI.Gtk.Button
    , fileChooserDialogOpenButton      :: GI.Gtk.Button
    , infoDialogButton                 :: GI.Gtk.Button
    , infoDialogCloseButton            :: GI.Gtk.Button
    , repeatCheckButton                :: GI.Gtk.CheckButton
    , volumeButton                     :: GI.Gtk.VolumeButton
    , playImage                        :: GI.Gtk.Image
    , pauseImage                       :: GI.Gtk.Image
    , fileChooserDialogButtonImage     :: GI.Gtk.Image
    , windowWidthSelectionComboBoxText :: GI.Gtk.ComboBoxText
    , videoSpeedSelectionComboboxText  :: GI.Gtk.ComboBoxText
    , subtitleSelectionComboBoxText    :: GI.Gtk.ComboBoxText
    , bufferingSpinner                 :: GI.Gtk.Spinner
    , errorMessageDialog               :: GI.Gtk.MessageDialog
    , infoDialog                       :: GI.Gtk.Dialog
    }

data IORefs =
  IORefs
    { isWindowFullScreenRef                  :: IORef Bool
    , mouseMovedLastRef                      :: IORef Integer
    , previousFileNamePathRef                :: IORef Data.Text.Text
    , videoInfoRef                           :: IORef VideoInfo
    , alteringBottomControlsBoxVisibilityRef :: IORef Bool
    }

data VideoInfo =
  VideoInfo
    { uri         :: Prelude.String
    , isLocalFile :: Bool
    , isVideo     :: Bool
    , isSeekable  :: Bool
    , videoWidth  :: Int
    , videoHeight :: Int
    }

data ScreensaverAndPowerManagementActions =
  ScreensaverAndPowerManagementActions
    { disabledPowerManagement     :: Bool
    , disabledXScreensaver        :: Bool
    , disabledCinnamonScreensaver :: Bool
    , disabledGnomeScreensaver    :: Bool
    }

defaultVideoInfo :: VideoInfo
defaultVideoInfo  =
  VideoInfo
    { uri         = ""
    , isLocalFile = False
    , isVideo     = False
    , isSeekable  = False
    , videoWidth  = 800
    , videoHeight = 600
    }

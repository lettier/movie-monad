{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

module Records where

import Data.IORef
import Data.Text
import qualified GI.Gtk
import qualified GI.Gst

data VideoInfo = VideoInfo {
      uri         :: Prelude.String
    , isLocalFile :: Bool
    , isVideo     :: Bool
    , isSeekable  :: Bool
    , videoWidth  :: Int
    , videoHeight :: Int
  }

defaultVideoInfo :: VideoInfo
defaultVideoInfo  = VideoInfo {
      uri         = ""
    , isLocalFile = False
    , isVideo     = False
    , isSeekable  = False
    , videoWidth  = 800
    , videoHeight = 600
  }

data GuiObjects = GuiObjects {
      window :: GI.Gtk.Window
    , fileChooserButton :: GI.Gtk.Button
    , fileChooserButtonLabel :: GI.Gtk.Label
    , fileChooserDialog :: GI.Gtk.Dialog
    , fileChooserEntry :: GI.Gtk.Entry
    , fileChooserWidget :: GI.Gtk.FileChooserWidget
    , fileChooserCancelButton :: GI.Gtk.Button
    , fileChooserOpenButton :: GI.Gtk.Button
    , videoWidget :: GI.Gtk.Widget
    , bottomControlsGtkBox :: GI.Gtk.Box
    , seekScale :: GI.Gtk.Scale
    , playPauseButton :: GI.Gtk.Button
    , playImage :: GI.Gtk.Image
    , pauseImage :: GI.Gtk.Image
    , volumeButton :: GI.Gtk.VolumeButton
    , videoWidthSelectionComboBox :: GI.Gtk.ComboBoxText
    , fullscreenButton :: GI.Gtk.Button
    , bufferingSpinner :: GI.Gtk.Spinner
    , errorMessageDialog :: GI.Gtk.MessageDialog
    , aboutButton :: GI.Gtk.Button
    , aboutDialog :: GI.Gtk.AboutDialog
  }

data IORefs = IORefs {
      isWindowFullScreenRef :: IORef Bool
    , mouseMovedLastRef :: IORef Integer
    , previousFileNamePathRef :: IORef Data.Text.Text
    , videoInfoRef :: IORef VideoInfo
  }

data Application = Application {
      guiObjects :: GuiObjects
    , ioRefs :: IORefs
    , playbin :: GI.Gst.Element
    , playbinBus :: GI.Gst.Bus
  }

{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

module MovieMonadLib where

import Data.IORef
import Data.Text
import qualified GI.Gtk

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
    , drawingAreaEventBox :: GI.Gtk.EventBox
    , drawingArea :: GI.Gtk.Widget
    , bottomControlsGtkBox :: GI.Gtk.Box
    , seekScale :: GI.Gtk.Scale
    , playPauseButton :: GI.Gtk.Button
    , playImage :: GI.Gtk.Image
    , pauseImage :: GI.Gtk.Image
    , volumeButton :: GI.Gtk.VolumeButton
    , desiredVideoWidthComboBox :: GI.Gtk.ComboBoxText
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

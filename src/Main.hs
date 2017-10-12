{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude
import Data.IORef
import Data.Maybe
import Data.Text
import Data.GI.Base
import Data.GI.Base.Properties
import GI.GObject
import qualified GI.Gtk
import GI.Gst
import GI.GstVideo
import GI.GdkPixbuf

import qualified Records as R
import Constants
import Window
import Mouse
import Keyboard
import FileChooser
import Seek
import PlayPause
import Fullscreen
import ErrorMessage
import About
import VideoSizeSelector
import Playbin
import Utils

import Paths_movie_monad

-- Declare Element a type instance of IsVideoOverlay via a newtype wrapper
-- Our GStreamer element is playbin
-- Playbin implements the GStreamer VideoOverlay interface
newtype GstElement = GstElement GI.Gst.Element
instance GI.GstVideo.IsVideoOverlay GstElement

main :: IO ()
main = do
  _ <- GI.Gst.init Nothing
  _ <- GI.Gtk.init Nothing

  gladeFile <- getDataFileName "data/gui.glade"
  builder <- GI.Gtk.builderNewFromFile (pack gladeFile)

  window <- builderGetObject GI.Gtk.Window builder "window"
  fileChooserButton <- builderGetObject GI.Gtk.Button builder "file-chooser-button"
  fileChooserButtonLabel <- builderGetObject GI.Gtk.Label builder "file-chooser-button-label"
  fileChooserDialog <- builderGetObject GI.Gtk.Dialog builder "file-chooser-dialog"
  fileChooserEntry <- builderGetObject GI.Gtk.Entry builder "file-chooser-entry"
  fileChooserWidget <- builderGetObject GI.Gtk.FileChooserWidget builder "file-chooser-widget"
  fileChooserCancelButton <- builderGetObject GI.Gtk.Button builder "file-chooser-cancel-button"
  fileChooserOpenButton <- builderGetObject GI.Gtk.Button builder "file-chooser-open-button"
  videoWidgetBox <- builderGetObject GI.Gtk.Box builder "video-widget-box"
  bottomControlsGtkBox <- builderGetObject GI.Gtk.Box builder "bottom-controls-gtk-box"
  seekScale <- builderGetObject GI.Gtk.Scale builder "seek-scale"
  playPauseButton <- builderGetObject GI.Gtk.Button builder "play-pause-button"
  playImage <- builderGetObject GI.Gtk.Image builder "play-image"
  pauseImage <- builderGetObject GI.Gtk.Image builder "pause-image"
  volumeButton <- builderGetObject GI.Gtk.VolumeButton builder "volume-button"
  videoWidthSelectionComboBox <- builderGetObject GI.Gtk.ComboBoxText builder "video-width-selection-combo-box"
  fullscreenButton <- builderGetObject GI.Gtk.Button builder "fullscreen-button"
  bufferingSpinner <- builderGetObject GI.Gtk.Spinner builder "buffering-spinner"
  errorMessageDialog <- builderGetObject GI.Gtk.MessageDialog builder "error-message-dialog"
  aboutButton <- builderGetObject GI.Gtk.Button builder "about-button"
  aboutDialog <- builderGetObject GI.Gtk.AboutDialog builder "about-dialog"

  logoFile <- getDataFileName "data/movie-monad-logo.svg"
  logo <- GI.GdkPixbuf.pixbufNewFromFile (pack logoFile)
  GI.Gtk.aboutDialogSetLogo aboutDialog (Just logo)

  -- Glade does not allow us to use the response ID nicknames so we setup them up programmatically here.
  GI.Gtk.dialogAddActionWidget fileChooserDialog fileChooserCancelButton (enumToInt32 GI.Gtk.ResponseTypeCancel)
  GI.Gtk.dialogAddActionWidget fileChooserDialog fileChooserOpenButton   (enumToInt32 GI.Gtk.ResponseTypeOk)

  isWindowFullScreenRef <- newIORef False
  mouseMovedLastRef <- newIORef 0
  previousFileNamePathRef <- newIORef ""
  videoInfoRef <- newIORef R.defaultVideoInfo

  let ioRefs = R.IORefs {
        R.isWindowFullScreenRef = isWindowFullScreenRef
      , R.mouseMovedLastRef = mouseMovedLastRef
      , R.previousFileNamePathRef = previousFileNamePathRef
      , R.videoInfoRef = videoInfoRef
    }

  playbin <- fromJust <$> GI.Gst.elementFactoryMake "playbin" (Just "MultimediaPlayerPlaybin")
  maybeGtkSink <- GI.Gst.elementFactoryMake "gtksink" (Just "MultimediaPlayerGtkSink")
  videoWidget <-
    case maybeGtkSink of
      Nothing -> do
        putStrLn "Could not create a GtkSink. Please install the bad plugins, version 1.8 or higher, for GStreamer 1."
        drawingArea <- GI.Gtk.drawingAreaNew
        GI.Gtk.widgetSetName drawingArea invalidVideoWidgetName
        GI.Gtk.unsafeCastTo GI.Gtk.Widget drawingArea
      Just gtkSink ->
        fromJust <$> Data.GI.Base.Properties.getObjectPropertyObject gtkSink "widget" GI.Gtk.Widget

  Data.GI.Base.Properties.setObjectPropertyObject playbin "video-sink" maybeGtkSink
  Data.GI.Base.Properties.setObjectPropertyBool   playbin "force-aspect-ratio" True
  GI.Gtk.boxPackStart videoWidgetBox videoWidget True True 0
  GI.Gtk.widgetSetHexpand videoWidget True
  GI.Gtk.widgetSetVexpand videoWidget True
  GI.Gtk.widgetSetSensitive videoWidget True

  playbinBus <- GI.Gst.elementGetBus playbin

  let guiObjects = R.GuiObjects {
        R.window = window
      , R.fileChooserButton = fileChooserButton
      , R.fileChooserButtonLabel = fileChooserButtonLabel
      , R.fileChooserDialog = fileChooserDialog
      , R.fileChooserEntry = fileChooserEntry
      , R.fileChooserWidget = fileChooserWidget
      , R.fileChooserCancelButton = fileChooserCancelButton
      , R.fileChooserOpenButton = fileChooserOpenButton
      , R.videoWidget = videoWidget
      , R.bottomControlsGtkBox = bottomControlsGtkBox
      , R.seekScale = seekScale
      , R.playPauseButton = playPauseButton
      , R.playImage = playImage
      , R.pauseImage = pauseImage
      , R.volumeButton = volumeButton
      , R.videoWidthSelectionComboBox = videoWidthSelectionComboBox
      , R.fullscreenButton = fullscreenButton
      , R.bufferingSpinner = bufferingSpinner
      , R.errorMessageDialog = errorMessageDialog
      , R.aboutButton = aboutButton
      , R.aboutDialog = aboutDialog
    }

  let application = R.Application {
        R.guiObjects = guiObjects
      , R.ioRefs = ioRefs
      , R.playbin = playbin
      , R.playbinBus = playbinBus
    }

  addWindowHandlers application
  addPlaybinHandler application
  addFileChooserHandlers application
  addPlayPauseButtonClickHandler application
  addSeekHandlers application
  addVideoSizeSelectorHandler application
  addFullscreenButtonReleaseHandler application
  addWindowMouseMoveHandlers application
  addAboutHandler application
  addKeyboardEventHandler application
  addErrorMessageDialogHandler application

  GI.Gtk.widgetShowAll window
  GI.Gtk.main

builderGetObject ::
  (GI.GObject.GObject b, GI.Gtk.IsBuilder a) =>
  (Data.GI.Base.ManagedPtr b -> b) ->
  a ->
  Prelude.String ->
  IO b
builderGetObject objectTypeClass builder objectId =
  fromJust <$> GI.Gtk.builderGetObject builder (pack objectId) >>=
    GI.Gtk.unsafeCastTo objectTypeClass

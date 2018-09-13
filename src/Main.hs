{-
  Movie Monad
  (C) 2017 David Lettier
  lettier.com
-}

{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
#-}

module Main where

import Prelude
import System.Info
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
import Capabilities
import Window
import CommandLine
import Mouse
import Keyboard
import FileChooser
import Seek
import PlayPause
import FullScreen
import ErrorMessage
import InfoDialog
import WindowWidthSelector
import SubtitleSelector
import VideoSpeedSelector
import Playbin
import ScreensaverAndPowerManagement (disable, enable)
import CssStyle
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
  builder   <- GI.Gtk.builderNewFromFile (pack gladeFile)

  window                           <- builderGetObject GI.Gtk.Window            builder "window"
  fileChooserDialogButtonLabel     <- builderGetObject GI.Gtk.Label             builder "file-chooser-dialog-button-label"
  fileChooserDialogLabel           <- builderGetObject GI.Gtk.Label             builder "file-chooser-dialog-label"
  videoLocationEntry               <- builderGetObject GI.Gtk.Entry             builder "video-location-entry"
  fileChooserWidget                <- builderGetObject GI.Gtk.FileChooserWidget builder "file-chooser-widget"
  videoWidgetBox                   <- builderGetObject GI.Gtk.Box               builder "video-widget-box"
  topControlsBox                   <- builderGetObject GI.Gtk.Box               builder "top-controls-box"
  bottomControlsBox                <- builderGetObject GI.Gtk.Box               builder "bottom-controls-box"
  seekScale                        <- builderGetObject GI.Gtk.Scale             builder "seek-scale"
  fileChooserDialogButton          <- builderGetObject GI.Gtk.Button            builder "file-chooser-dialog-button"
  fileChooserDialogCancelButton    <- builderGetObject GI.Gtk.Button            builder "file-chooser-dialog-cancel-button"
  fileChooserDialogOpenButton      <- builderGetObject GI.Gtk.Button            builder "file-chooser-dialog-open-button"
  playPauseButton                  <- builderGetObject GI.Gtk.Button            builder "play-pause-button"
  fullScreenButton                 <- builderGetObject GI.Gtk.Button            builder "full-screen-button"
  infoDialogButton                 <- builderGetObject GI.Gtk.Button            builder "info-dialog-button"
  repeatCheckButton                <- builderGetObject GI.Gtk.CheckButton       builder "repeat-check-button"
  volumeButton                     <- builderGetObject GI.Gtk.VolumeButton      builder "volume-button"
  playImage                        <- builderGetObject GI.Gtk.Image             builder "play-image"
  pauseImage                       <- builderGetObject GI.Gtk.Image             builder "pause-image"
  fileChooserDialogButtonImage     <- builderGetObject GI.Gtk.Image             builder "file-chooser-dialog-button-image"
  windowWidthSelectionComboBoxText <- builderGetObject GI.Gtk.ComboBoxText      builder "window-width-selection-combo-box-text"
  videoSpeedSelectionComboboxText  <- builderGetObject GI.Gtk.ComboBoxText      builder "video-speed-selection-combo-box-text"
  subtitleSelectionComboBoxText    <- builderGetObject GI.Gtk.ComboBoxText      builder "subtitle-selection-combo-box-text"
  bufferingSpinner                 <- builderGetObject GI.Gtk.Spinner           builder "buffering-spinner"
  fileChooserDialog                <- builderGetObject GI.Gtk.Dialog            builder "file-chooser-dialog"
  errorMessageDialog               <- builderGetObject GI.Gtk.MessageDialog     builder "error-message-dialog"
  infoDialog                       <- builderGetObject GI.Gtk.AboutDialog       builder "info-dialog"

  logoFile <- getDataFileName "data/movie-monad-logo.svg"
  logo <- GI.GdkPixbuf.pixbufNewFromFile logoFile
  GI.Gtk.aboutDialogSetLogo infoDialog (Just logo)

  -- Glade does not allow us to use the response ID nicknames so we programmatically set them here.
  GI.Gtk.dialogAddActionWidget fileChooserDialog fileChooserDialogCancelButton (enumToInt32 GI.Gtk.ResponseTypeCancel)
  GI.Gtk.dialogAddActionWidget fileChooserDialog fileChooserDialogOpenButton   (enumToInt32 GI.Gtk.ResponseTypeOk)

  isWindowFullScreenRef                  <- newIORef False
  mouseMovedLastRef                      <- newIORef 0
  previousFileNamePathRef                <- newIORef ""
  videoInfoRef                           <- newIORef R.defaultVideoInfo
  alteringBottomControlsBoxVisibilityRef <- newIORef False

  let ioRefs =
        R.IORefs
          { R.isWindowFullScreenRef                  = isWindowFullScreenRef
          , R.mouseMovedLastRef                      = mouseMovedLastRef
          , R.previousFileNamePathRef                = previousFileNamePathRef
          , R.videoInfoRef                           = videoInfoRef
          , R.alteringBottomControlsBoxVisibilityRef = alteringBottomControlsBoxVisibilityRef
          }

  playbin <- fromJust <$> GI.Gst.elementFactoryMake "playbin" (Just "MultimediaPlayerPlaybin")
  maybeGtkSink <- GI.Gst.elementFactoryMake "gtksink" (Just "MultimediaPlayerGtkSink")
  videoWidget <-
    case maybeGtkSink of
      Nothing -> do
        putStrLn "[ERROR] Could not create a \"gtksink\". Please install the GStreamer 1.0 bad plugins version 1.8 or higher."
        drawingArea <- GI.Gtk.drawingAreaNew
        GI.Gtk.widgetSetName drawingArea invalidVideoWidgetName
        GI.Gtk.unsafeCastTo GI.Gtk.Widget drawingArea
      Just gtkSink ->
        fromJust <$> Data.GI.Base.Properties.getObjectPropertyObject gtkSink "widget" GI.Gtk.Widget

  Data.GI.Base.Properties.setObjectPropertyObject playbin "video-sink" maybeGtkSink
  Data.GI.Base.Properties.setObjectPropertyBool   playbin "force-aspect-ratio" True
  GI.Gtk.boxPackStart videoWidgetBox videoWidget True True 0
  GI.Gtk.widgetSetHexpand   videoWidget True
  GI.Gtk.widgetSetVexpand   videoWidget True
  GI.Gtk.widgetSetSensitive videoWidget True

  turnOffSubtitles playbin

  playbinBus <- fromJust <$> GI.Gst.elementGetBus playbin

  let guiObjects =
        R.GuiObjects
          { R.window                           = window
          , R.fileChooserDialogButtonLabel     = fileChooserDialogButtonLabel
          , R.fileChooserDialogLabel           = fileChooserDialogLabel
          , R.videoLocationEntry               = videoLocationEntry
          , R.fileChooserWidget                = fileChooserWidget
          , R.fileChooserDialogCancelButton    = fileChooserDialogCancelButton
          , R.fileChooserDialogOpenButton      = fileChooserDialogOpenButton
          , R.videoWidget                      = videoWidget
          , R.topControlsBox                   = topControlsBox
          , R.bottomControlsBox                = bottomControlsBox
          , R.seekScale                        = seekScale
          , R.fileChooserDialogButton          = fileChooserDialogButton
          , R.playPauseButton                  = playPauseButton
          , R.fullScreenButton                 = fullScreenButton
          , R.volumeButton                     = volumeButton
          , R.repeatCheckButton                = repeatCheckButton
          , R.playImage                        = playImage
          , R.pauseImage                       = pauseImage
          , R.fileChooserDialogButtonImage     = fileChooserDialogButtonImage
          , R.windowWidthSelectionComboBoxText = windowWidthSelectionComboBoxText
          , R.videoSpeedSelectionComboboxText  = videoSpeedSelectionComboboxText
          , R.subtitleSelectionComboBoxText    = subtitleSelectionComboBoxText
          , R.bufferingSpinner                 = bufferingSpinner
          , R.errorMessageDialog               = errorMessageDialog
          , R.fileChooserDialog                = fileChooserDialog
          , R.infoDialogButton                 = infoDialogButton
          , R.infoDialog                       = infoDialog
          }

  let application =
        R.Application
          { R.guiObjects = guiObjects
          , R.ioRefs = ioRefs
          , R.playbin = playbin
          , R.playbinBus = playbinBus
          }

  addWindowHandlers                 application [playVideoFromCommandLineIfNeeded]
  addPlaybinHandlers                application
  addFileChooserHandlers            application
  addPlayPauseButtonClickHandler    application
  addSeekHandlers                   application
  addWindowWidthSelectorHandlers    application
  addVideoSpeedSelectionHandlers    application
  addSubtitleSelectorHandler        application
  addFullScreenButtonReleaseHandler application
  addMouseMoveHandlers              application [fillWindowWithVideo]
  addInfoDialogHandler              application
  addKeyboardEventHandler           application
  addErrorMessageDialogHandler      application

  let operatingSystem = System.Info.os
  screenAndPowerManagementActions <- ScreensaverAndPowerManagement.disable operatingSystem

  applyCss guiObjects

  checkCapabilitiesAndNotify application

  GI.Gtk.widgetShowAll window

  GI.Gtk.main

  ScreensaverAndPowerManagement.enable operatingSystem screenAndPowerManagementActions

builderGetObject
  ::  (GI.GObject.GObject b, GI.Gtk.IsBuilder a)
  =>  (Data.GI.Base.ManagedPtr b -> b)
  ->  a
  ->  Prelude.String
  ->  IO b
builderGetObject objectTypeClass builder objectId =
  fromJust <$> GI.Gtk.builderGetObject builder (pack objectId) >>=
    GI.Gtk.unsafeCastTo objectTypeClass

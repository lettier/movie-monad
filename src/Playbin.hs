{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Playbin where

import Control.Monad
import Data.Text
import Data.GI.Base.Properties
import GI.GLib
import qualified GI.Gtk
import qualified GI.Gst

import qualified Records as R
import Window
import PlayPause
import ErrorMessage
import Uri

addPlaybinHandler :: R.Application -> IO ()
addPlaybinHandler
  R.Application {
        R.guiObjects = guiObjects@R.GuiObjects {
              R.volumeButton = volumeButton
          }
      , R.playbin = playbin
      , R.playbinBus = playbinBus
    }
  =
  void(
      GI.Gst.busAddWatch playbinBus GI.GLib.PRIORITY_DEFAULT (
          pipelineBusMessageHandler
            guiObjects
            playbin
        )
    ) >>
  void (
      GI.Gtk.onScaleButtonValueChanged volumeButton (volumeButtonValueChangedHandler playbin)
    )

pipelineBusMessageHandler ::
  R.GuiObjects ->
  GI.Gst.Element ->
  GI.Gst.Bus ->
  GI.Gst.Message ->
  IO Bool
pipelineBusMessageHandler
  guiObjects@R.GuiObjects {
        R.seekScale = seekScale
      , R.fileChooserEntry = fileChooserEntry
      , R.fileChooserButtonLabel = fileChooserButtonLabel
      , R.volumeButton = volumeButton
      , R.errorMessageDialog = errorMessageDialog
      , R.bufferingSpinner = bufferingSpinner
      , R.playPauseButton = playPauseButton
    }
  playbin
  _
  message
  = do
  messageTypes <- GI.Gst.getMessageType message
  let messageType = case messageTypes of
                      [] -> GI.Gst.MessageTypeUnknown
                      (msg:_) -> msg
  entryText <- GI.Gtk.entryGetText fileChooserEntry
  labelText <- GI.Gtk.labelGetText fileChooserButtonLabel
  when (
      messageType == GI.Gst.MessageTypeError &&
      (
        (not . Data.Text.null) entryText ||
        labelText /= "Open"
      )
    ) $ do
      (gError, text) <- GI.Gst.messageParseError message
      gErrorText <- GI.Gst.gerrorMessage gError
      Prelude.mapM_ print [text, "\n", gErrorText]
      GI.Gtk.entrySetText fileChooserEntry ""
      GI.Gtk.labelSetText fileChooserButtonLabel "Open"
      _ <- GI.Gst.elementSetState playbin GI.Gst.StateNull
      setPlaybinUriAndVolume playbin "" volumeButton
      resetWindow guiObjects
      runErrorMessageDialog
        errorMessageDialog
        (Data.Text.concat ["There was a problem trying to play the video \"", entryText, "\"."])
  when (messageType == GI.Gst.MessageTypeBuffering) $ do
    percent <- GI.Gst.messageParseBuffering message
    isPlaying <- isPlayPauseButtonPlaying playPauseButton
    if percent >= 100
      then do
        GI.Gtk.widgetHide bufferingSpinner
        GI.Gtk.setSpinnerActive bufferingSpinner False
        GI.Gtk.widgetSetSensitive seekScale True
        when isPlaying $ void $ GI.Gst.elementSetState playbin GI.Gst.StatePlaying
      else do
        GI.Gtk.widgetShow bufferingSpinner
        GI.Gtk.setSpinnerActive bufferingSpinner True
        GI.Gtk.widgetSetSensitive seekScale False
        void $ GI.Gst.elementSetState playbin GI.Gst.StatePaused
    return ()
  return True

volumeButtonValueChangedHandler ::
  GI.Gst.Element ->
  Double ->
  IO ()
volumeButtonValueChangedHandler playbin volume =
    void $ Data.GI.Base.Properties.setObjectPropertyDouble playbin "volume" volume

setPlaybinUriAndVolume ::
  GI.Gst.Element ->
  Prelude.String ->
  GI.Gtk.VolumeButton ->
  IO ()
setPlaybinUriAndVolume playbin fileName volumeButton = do
  uri <- addUriSchemeIfNone fileName
  volume <- GI.Gtk.scaleButtonGetValue volumeButton
  Data.GI.Base.Properties.setObjectPropertyDouble playbin "volume" volume
  Data.GI.Base.Properties.setObjectPropertyString playbin "uri" (Just $ pack uri)

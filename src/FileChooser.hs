{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}

module FileChooser where

import Control.Monad
import Data.Int
import Data.Text
import Data.IORef
import qualified Network.URI
import qualified GI.Gdk
import qualified GI.Gtk
import qualified GI.Gst

import qualified Records as R
import Constants
import Window
import PlayPause
import ErrorMessage
import VideoInfo
import Playbin
import Uri
import Utils

addFileChooserHandlers :: R.Application -> IO ()
addFileChooserHandlers
  application@R.Application {
        R.guiObjects = R.GuiObjects {
              R.fileChooserButton = fileChooserButton
            , R.fileChooserDialog = fileChooserDialog
            , R.fileChooserWidget = fileChooserWidget
            , R.fileChooserEntry = fileChooserEntry
          }
    }
  =
  void (GI.Gtk.onWidgetButtonReleaseEvent fileChooserButton (fileChooserButtonClickHandler application)) >>
  void (GI.Gtk.onDialogResponse fileChooserDialog (fileChooserDialogResponseHandler application)) >>
  void (GI.Gtk.onFileChooserSelectionChanged fileChooserWidget (fileChooserSelectionChangedHandler application)) >>
  void (GI.Gtk.onEntryIconRelease fileChooserEntry (\ _ _ -> GI.Gtk.entrySetText fileChooserEntry ""))

fileChooserDialogResponseHandler ::
  R.Application ->
  Int32 ->
  IO ()
fileChooserDialogResponseHandler
  R.Application {
        R.guiObjects = guiObjects@R.GuiObjects {
              R.window = window
            , R.fileChooserButton = fileChooserButton
            , R.videoWidget = videoWidget
            , R.seekScale = seekScale
            , R.playPauseButton = playPauseButton
            , R.videoWidthSelectionComboBox = videoWidthSelectionComboBox
            , R.fullscreenButton = fullscreenButton
            , R.fileChooserEntry = fileChooserEntry
            , R.fileChooserButtonLabel = fileChooserButtonLabel
            , R.volumeButton = volumeButton
            , R.errorMessageDialog = errorMessageDialog
            , R.fileChooserDialog = fileChooserDialog
            , R.playImage = playImage
            , R.pauseImage = pauseImage
          }
      , R.ioRefs = R.IORefs {
              R.isWindowFullScreenRef = isWindowFullScreenRef
            , R.videoInfoRef = videoInfoRef
            , R.previousFileNamePathRef = previousFileNamePathRef
          }
      , R.playbin = playbin
    }
  responseId
  = do
  GI.Gtk.widgetHide fileChooserDialog
  handleResponseType GI.Gtk.ResponseTypeOk
  where
    handleResponseType :: (Enum a, Ord a) => a -> IO ()
    handleResponseType enum
      | enumToInt32 enum == responseId = do
        _ <- GI.Gst.elementSetState playbin GI.Gst.StateNull
        filePathName <- GI.Gtk.entryGetText fileChooserEntry
        let filePathNameStr = Data.Text.unpack filePathName
        (_, fileNameEmpty) <- setFileChooserButtonLabel fileChooserButtonLabel filePathName
        isWindowFullScreen <- readIORef isWindowFullScreenRef
        videoWidthSelection <- getSelectedVideoWidth videoWidthSelectionComboBox
        setPlaybinUriAndVolume playbin filePathNameStr volumeButton
        handleFileName
          fileNameEmpty
          filePathNameStr
          isWindowFullScreen
          videoWidthSelection
      | otherwise = do
        filePathName <- readIORef previousFileNamePathRef
        _ <- setFileChooserButtonLabel fileChooserButtonLabel filePathName
        GI.Gtk.entrySetText fileChooserEntry filePathName
    handleFileName ::
      Bool ->
      Prelude.String ->
      Bool ->
      Int ->
      IO ()
    handleFileName True _ _ _ = atomicWriteIORef videoInfoRef R.defaultVideoInfo >> resetWindow guiObjects
    handleFileName
      _
      filePathNameStr
      isWindowFullScreen
      videoWidthSelection
      = do
      retrievedVideoInfo <- getVideoInfo videoInfoRef filePathNameStr
      maybeWindowSize <- calculateWindowSize videoWidthSelection retrievedVideoInfo
      GI.Gtk.widgetSetSensitive seekScale True
      if R.isSeekable retrievedVideoInfo
        then GI.Gtk.widgetShow seekScale
        else GI.Gtk.widgetHide seekScale
      case maybeWindowSize of
        Nothing -> do
          resetWindow guiObjects
          runErrorMessageDialog
            errorMessageDialog
            (Data.Text.pack $ Prelude.concat ["\"", filePathNameStr, "\" is not a video."])
        Just (width, height) -> do
          videoWidgetName <- GI.Gtk.widgetGetName videoWidget
          if videoWidgetName == invalidVideoWidgetName
            then do
              resetWindow guiObjects
              runErrorMessageDialog
                errorMessageDialog
                "Cannot play the video. Please install the bad plugins, version 1.8 or higher, for GStreamer version 1."
            else do
              GI.Gtk.widgetShow videoWidget
              GI.Gtk.widgetShow playPauseButton
              GI.Gtk.widgetShow fullscreenButton
              setPlayPauseButton playPauseButton playImage pauseImage True
              unless isWindowFullScreen $
                setWindowSize width height fileChooserButton videoWidget window
              void $ GI.Gst.elementSetState playbin GI.Gst.StatePlaying

fileChooserButtonClickHandler ::
  R.Application ->
  GI.Gdk.EventButton ->
  IO Bool
fileChooserButtonClickHandler
  R.Application {
        R.guiObjects = R.GuiObjects {
              R.fileChooserEntry = fileChooserEntry
            , R.fileChooserDialog = fileChooserDialog
          }
      , R.ioRefs = R.IORefs {
              R.previousFileNamePathRef = previousFileNamePathRef
          }
    }
  _
  = do
  text <- GI.Gtk.entryGetText fileChooserEntry
  atomicWriteIORef previousFileNamePathRef text
  _ <- GI.Gtk.dialogRun fileChooserDialog
  return True

fileChooserSelectionChangedHandler ::
  R.Application ->
  IO ()
fileChooserSelectionChangedHandler
  R.Application {
        R.guiObjects = R.GuiObjects {
              R.fileChooserWidget = fileChooserWidget
            , R.fileChooserEntry = fileChooserEntry
          }
      , R.ioRefs = R.IORefs {
              R.videoInfoRef = videoInfoRef
          }
    }
  = do
  maybeUri <- GI.Gtk.fileChooserGetUri fileChooserWidget
  case maybeUri of
    Nothing -> return ()
    Just uri' -> do
      let uri = Network.URI.unEscapeString $ Data.Text.unpack uri'
      local <- isLocalFile uri
      video <- R.isVideo <$> getVideoInfo videoInfoRef uri
      GI.Gtk.entrySetText fileChooserEntry (if local && video then Data.Text.pack uri else "")

setFileChooserButtonLabel :: GI.Gtk.Label -> Data.Text.Text -> IO (Data.Text.Text, Bool)
setFileChooserButtonLabel fileChooserButtonLabel filePathName = do
  let fileName = fileNameFromFilePathName filePathName
  let fileNameEmpty = isTextEmpty fileName
  GI.Gtk.labelSetText fileChooserButtonLabel (if fileNameEmpty then "Open" else fileName)
  return (fileName, fileNameEmpty)

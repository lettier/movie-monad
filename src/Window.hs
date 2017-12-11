{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Window where

import GHC.Word
import Control.Monad
import Data.Int
import Data.IORef
import Data.Time.Clock.POSIX
import GI.GLib
import qualified GI.Gdk
import qualified GI.Gtk
import qualified GI.Gst

import qualified Records as R
import Constants
import Mouse
import PlayPause
import Utils

addWindowHandlers :: R.Application -> [R.Application -> IO ()] -> IO ()
addWindowHandlers
  application@R.Application {
        R.guiObjects = R.GuiObjects {
              R.window = window
            , R.videoWidget = videoWidget
          }
      , R.ioRefs = R.IORefs {
              R.isWindowFullScreenRef = isWindowFullScreenRef
          }
      , R.playbin = playbin
    }
  functionsToRunOnWindowRealized
  =
  void (
      GI.Gtk.onWidgetRealize videoWidget (
          windowRealizedHandler
            application
            functionsToRunOnWindowRealized
        )
    ) >>
  void (
      GI.Gtk.onWidgetWindowStateEvent window (widgetWindowStateEventHandler isWindowFullScreenRef)
    ) >>
  void (
      GI.Gtk.onWidgetDestroy window (windowDestroyHandler playbin)
    ) >>
  void (
      GI.GLib.timeoutAddSeconds GI.GLib.PRIORITY_DEFAULT 1 (hideOnScreenControls application)
    )

windowRealizedHandler ::
  R.Application ->
  [R.Application -> IO ()] ->
  GI.Gtk.WidgetRealizeCallback
windowRealizedHandler
  application@R.Application {
        R.guiObjects = guiObjects@R.GuiObjects {
              R.videoWidget = videoWidget
            , R.seekScale = seekScale
          }
    }
  functionsToRunOnWindowRealized
  = do
  let eventMask = enumToInt32 GI.Gdk.EventMaskAllEventsMask
  GI.Gtk.widgetAddEvents videoWidget eventMask
  GI.Gtk.widgetAddEvents seekScale eventMask
  resetWindow guiObjects
  mapM_ (\ f -> f application) functionsToRunOnWindowRealized

widgetWindowStateEventHandler ::
  IORef Bool ->
  GI.Gdk.EventWindowState ->
  IO Bool
widgetWindowStateEventHandler isWindowFullScreenRef eventWindowState = do
  windowStates <- GI.Gdk.getEventWindowStateNewWindowState eventWindowState
  let isWindowFullScreen = Prelude.foldl (\ acc x ->
          acc || GI.Gdk.WindowStateFullscreen == x
        ) False windowStates
  atomicWriteIORef isWindowFullScreenRef isWindowFullScreen
  return True

windowDestroyHandler ::
  GI.Gst.Element ->
  IO ()
windowDestroyHandler playbin = do
  _ <- GI.Gst.elementSetState playbin GI.Gst.StateNull
  _ <- GI.Gst.objectUnref playbin
  GI.Gtk.mainQuit

hideOnScreenControls ::
  R.Application ->
  IO Bool
hideOnScreenControls
  R.Application {
        R.guiObjects = R.GuiObjects {
              R.window = window
            , R.fileChooserButton = fileChooserButton
            , R.bottomControlsGtkBox = bottomControlsGtkBox
          }
      , R.ioRefs = R.IORefs {
            R.videoInfoRef = videoInfoRef
          , R.mouseMovedLastRef = mouseMovedLastRef
        }
      , R.playbin = playbin
    }
  = do
  isVideo <- R.isVideo <$> readIORef videoInfoRef
  mouseMovedLast <- readIORef mouseMovedLastRef
  timeNow <- fmap round getPOSIXTime
  (_, playBinState, _) <- GI.Gst.elementGetState playbin (fromIntegral GI.Gst.MSECOND :: GHC.Word.Word64)
  let isPlaying = GI.Gst.StatePlaying == playBinState
  let delta = timeNow - mouseMovedLast
  when (isPlaying && isVideo && delta >= hideOnScreenControlsInterval) $ do
    GI.Gtk.widgetHide fileChooserButton
    GI.Gtk.widgetHide bottomControlsGtkBox
    setCursor window (Just "none")
    atomicWriteIORef mouseMovedLastRef timeNow
  return True

calculateWindowSize :: Int -> R.VideoInfo -> IO (Maybe (Int32, Int32))
calculateWindowSize videoWidthSelection retrievedVideoInfo =
  widthHeightToDouble retrievedVideoInfo >>=
  ratio >>=
  windowSize
  where
    widthHeightToDouble :: R.VideoInfo -> IO (Maybe Double, Maybe Double)
    widthHeightToDouble R.VideoInfo { R.isVideo = False } = return (Nothing, Nothing)
    widthHeightToDouble R.VideoInfo { R.videoWidth = w, R.videoHeight = h } =
      return (Just $ fromIntegral w :: Maybe Double, Just $ fromIntegral h :: Maybe Double)
    ratio :: (Maybe Double, Maybe Double) -> IO (Maybe Double)
    ratio (Just width, Just height) =
      if width <= 0.0 then return Nothing else return (Just (height / width))
    ratio _ = return Nothing
    windowSize :: Maybe Double -> IO (Maybe (Int32, Int32))
    windowSize Nothing = return Nothing
    windowSize (Just ratio') =
      return (
          Just (
                fromIntegral videoWidthSelection :: Int32
              , round ((fromIntegral videoWidthSelection :: Double) *  ratio') :: Int32
            )
        )

setWindowSize ::
  Int32 ->
  Int32 ->
  GI.Gtk.Button ->
  GI.Gtk.Widget ->
  GI.Gtk.Window ->
  IO ()
setWindowSize width height fileChooserButton videoWidget window = do
  GI.Gtk.setWidgetWidthRequest fileChooserButton width
  GI.Gtk.setWidgetWidthRequest videoWidget width
  GI.Gtk.setWidgetHeightRequest videoWidget height
  GI.Gtk.setWidgetWidthRequest window width
  GI.Gtk.setWidgetHeightRequest window height
  GI.Gtk.windowResize window width (if height <= 0 then 1 else height)

resetWindow ::
  R.GuiObjects ->
  IO ()
resetWindow
  R.GuiObjects {
        R.window = window
      , R.fileChooserButton = fileChooserButton
      , R.videoWidget = videoWidget
      , R.seekScale = seekScale
      , R.playPauseButton = playPauseButton
      , R.videoWidthSelectionComboBox = videoWidthSelectionComboBox
      , R.fullscreenButton = fullscreenButton
      , R.playImage = playImage
      , R.pauseImage = pauseImage
      , R.bottomControlsGtkBox = bottomControlsGtkBox
    }
  = do
  videoWidthSelection <- getSelectedVideoWidth videoWidthSelectionComboBox
  let width = fromIntegral videoWidthSelection :: Int32
  GI.Gtk.windowUnfullscreen window
  GI.Gtk.widgetHide videoWidget
  GI.Gtk.widgetHide seekScale
  GI.Gtk.widgetHide playPauseButton
  GI.Gtk.widgetHide fullscreenButton
  GI.Gtk.widgetShow fileChooserButton
  GI.Gtk.widgetShow bottomControlsGtkBox
  GI.Gtk.widgetShow videoWidthSelectionComboBox
  setCursor window Nothing
  setPlayPauseButton playPauseButton playImage pauseImage False
  setWindowSize width 0 fileChooserButton videoWidget window

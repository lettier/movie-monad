{-
  Movie Monad
  (C) 2017 David Lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Window where

import GHC.Word
import Control.Monad
import Data.Maybe
import Data.Int
import Data.IORef
import Data.Time.Clock.POSIX
import GI.GLib
import qualified GI.Gdk
import qualified GI.Gtk
import qualified GI.Gst

import qualified Records as R
import Constants
import Reset
import Mouse
import PlayPause
import Seek
import BottomControlsBoxVisibility
import Utils

addWindowHandlers :: R.Application -> [R.Application -> IO ()] -> IO ()
addWindowHandlers
  application@R.Application
    { R.guiObjects =
        R.GuiObjects
          { R.window      = window
          , R.videoWidget = videoWidget
          }
    , R.ioRefs =
        R.IORefs
          { R.isWindowFullScreenRef = isWindowFullScreenRef
          }
    , R.playbin = playbin
    }
  onWidgetRealizeCallbacks
  = do
  void $
    GI.Gtk.onWidgetRealize
      videoWidget $
        windowRealizedHandler
          application
          onWidgetRealizeCallbacks
  void $
    GI.Gtk.onWidgetWindowStateEvent
      window $
        widgetWindowStateEventHandler isWindowFullScreenRef
  void $
    GI.Gtk.onWidgetDestroy
      window $
        windowDestroyHandler playbin
  void $
    GI.GLib.timeoutAddSeconds
      GI.GLib.PRIORITY_DEFAULT 1 $
        adjustWindowPeriodically application

windowRealizedHandler
  ::  R.Application
  ->  [R.Application -> IO ()]
  ->  GI.Gtk.WidgetRealizeCallback
windowRealizedHandler
  application@R.Application
    { R.guiObjects =
        guiObjects@R.GuiObjects
          { R.fileChooserButton = fileChooserButton
          , R.videoWidget       = videoWidget
          , R.seekScale         = seekScale
          }
    }
  onWidgetRealizeCallbacks
  = do
  let eventMask = [GI.Gdk.EventMaskAllEventsMask]
  GI.Gtk.widgetAddEvents fileChooserButton eventMask
  GI.Gtk.widgetAddEvents videoWidget       eventMask
  GI.Gtk.widgetAddEvents seekScale         eventMask
  resetGuiObjects guiObjects
  mapM_ (\ f -> f application) onWidgetRealizeCallbacks

widgetWindowStateEventHandler :: IORef Bool -> GI.Gdk.EventWindowState -> IO Bool
widgetWindowStateEventHandler isWindowFullScreenRef eventWindowState = do
  windowStates <- GI.Gdk.getEventWindowStateNewWindowState eventWindowState
  let isWindowFullScreen =
        Prelude.foldl
          (\ acc x -> acc || GI.Gdk.WindowStateFullscreen == x)
          False
          windowStates
  atomicWriteIORef isWindowFullScreenRef isWindowFullScreen
  return True

windowDestroyHandler :: GI.Gst.Element -> IO ()
windowDestroyHandler playbin = do
  _ <- GI.Gst.elementSetState playbin GI.Gst.StateNull
  _ <- GI.Gst.objectUnref playbin
  GI.Gtk.mainQuit

adjustWindowPeriodically :: R.Application -> IO Bool
adjustWindowPeriodically
  application@R.Application
    { R.guiObjects =
        R.GuiObjects
          { R.window         = window
          , R.topControlsBox = topControlsBox
          }
    , R.ioRefs =
        R.IORefs
          { R.videoInfoRef      = videoInfoRef
          , R.mouseMovedLastRef = mouseMovedLastRef
          }
    , R.playbin = playbin
    }
  = do
  videoInfo            <- readIORef videoInfoRef
  mouseMovedLast       <- readIORef mouseMovedLastRef
  timeNow              <- fmap round getPOSIXTime
  (_, playBinState, _) <- GI.Gst.elementGetState playbin (fromIntegral GI.Gst.MSECOND :: GHC.Word.Word64)
  let isPlaying        = GI.Gst.StatePlaying == playBinState
  let delta            = timeNow - mouseMovedLast
  let isVideo          = R.isVideo videoInfo
  when (isPlaying && isVideo && delta >= hideOnScreenControlsInterval) $ do
    GI.Gtk.widgetHide topControlsBox
    setCursor window (Just "none")
    void $ fadeOutBottomControlsBox application
  fillWindowWithVideo application
  return True

fillWindowWithVideo :: R.Application -> IO ()
fillWindowWithVideo
  R.Application
    { R.guiObjects =
        guiObjects@R.GuiObjects
          { R.window = window
          }
    , R.ioRefs =
        R.IORefs
          { R.videoInfoRef          = videoInfoRef
          , R.isWindowFullScreenRef = isWindowFullScreenRef
          }
    }
  = do
  isWindowFullScreen   <- readIORef isWindowFullScreenRef
  videoInfo            <- readIORef videoInfoRef
  when (R.isVideo videoInfo && not isWindowFullScreen) $ do
    (width, _)      <- GI.Gtk.windowGetSize window
    maybeWindowSize <- calculateWindowSize guiObjects (fromIntegral width :: Int) videoInfo
    when (isJust maybeWindowSize) $ do
      let (windowWidth, windowHeight) = fromMaybe (0, 0) maybeWindowSize
      setWindowSize guiObjects windowWidth windowHeight
  return ()

calculateWindowSize :: R.GuiObjects -> Int -> R.VideoInfo -> IO (Maybe (Int32, Int32))
calculateWindowSize _ _ R.VideoInfo { R.isVideo = False } = return Nothing
calculateWindowSize
  R.GuiObjects
    { R.topControlsBox = topControlsBox
    }
  desiredWidth
  R.VideoInfo
    { R.videoWidth  = videoWidth
    , R.videoHeight = videoHeight
    }
  = do
  topControlsBoxIsVisible <- GI.Gtk.widgetGetVisible topControlsBox
  topControlsBoxHeight    <- GI.Gtk.widgetGetAllocation topControlsBox >>= GI.Gdk.getRectangleHeight
  let videoWidthDouble       = fromIntegral videoWidth  :: Double
  let videoHeightDouble      = fromIntegral videoHeight :: Double
  let ratio =
        if videoWidthDouble <= 0.0
          then 0.0
          else videoHeightDouble / videoWidthDouble
  let desiredWidthDouble = fromIntegral desiredWidth :: Double
  let topMargin =
        if topControlsBoxIsVisible
          then (fromIntegral topControlsBoxHeight :: Double)
          else 0.0
  let bottomMargin = 0.0
  let height = topMargin + (desiredWidthDouble * ratio) + bottomMargin
  return $ Just (fromIntegral desiredWidth:: Int32, round height :: Int32)

setWindowSize :: R.GuiObjects -> Int32 -> Int32 -> IO ()
setWindowSize
  R.GuiObjects
    { R.window = window
    }
  width
  height
  =
  GI.Gtk.windowResize window width (if height <= 0 then 1 else height)

setupWindowForPlayback :: R.Application -> Bool -> Bool -> Int32 -> Int32 -> IO ()
setupWindowForPlayback
  application@R.Application
    { R.guiObjects =
        guiObjects@R.GuiObjects
          { R.window                          = window
          , R.videoWidget                     = videoWidget
          , R.topControlsBox                  = topControlsBox
          , R.bottomControlsBox               = bottomControlsBox
          , R.seekScale                       = seekScale
          , R.playPauseButton                 = playPauseButton
          , R.repeatCheckButton               = repeatCheckButton
          , R.fullscreenButton                = fullscreenButton
          , R.videoSpeedSelectionComboboxText = videoSpeedSelectionComboboxText
          , R.subtitleSelectionComboBoxText   = subtitleSelectionComboBoxText
          , R.playImage                       = playImage
          , R.pauseImage                      = pauseImage
          }
    }
  isSeekable
  isWindowFullScreen
  windowWidth
  windowHeight
  = do
  GI.Gtk.widgetShow videoWidget
  GI.Gtk.widgetShow playPauseButton
  GI.Gtk.widgetShow repeatCheckButton
  GI.Gtk.widgetShow fullscreenButton
  GI.Gtk.setToggleButtonActive repeatCheckButton False
  windowMinimumHeight <- getWindowMinimumHeight
  GI.Gtk.widgetSetSizeRequest window windowMinimumWidth windowMinimumHeight
  GI.Gtk.comboBoxSetActive videoSpeedSelectionComboboxText 1
  GI.Gtk.comboBoxSetActive subtitleSelectionComboBoxText 0
  setPlayPauseButton playPauseButton playImage pauseImage True
  unless isWindowFullScreen $
    setWindowSize guiObjects windowWidth windowHeight
  hideOrShowSeekScale seekScale isSeekable
  void $ fadeInBottomControlsBox application 0
  where
    getWindowMinimumHeight :: IO Int32
    getWindowMinimumHeight = do
      (_, topControlsboxHeight)     <- getWidgetSize topControlsBox
      (_, bottomControlsBoxHeight)  <- getWidgetSize bottomControlsBox
      bottomControlsBoxMarginTop    <- GI.Gtk.widgetGetMarginTop bottomControlsBox
      bottomControlsBoxMarginBottom <- GI.Gtk.widgetGetMarginBottom bottomControlsBox
      return $
          topControlsboxHeight
        + bottomControlsBoxHeight
        + bottomControlsBoxMarginTop
        + bottomControlsBoxMarginBottom

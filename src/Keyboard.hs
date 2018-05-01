{-
  Movie Monad
  (C) 2017 David Lettier
  lettier.com
-}

module Keyboard where

import Control.Monad
import Data.IORef
import qualified GI.Gdk
import qualified GI.Gtk

import qualified Records as R
import Window
import Mouse
import PlayPause
import FullScreen
import Constants
import Utils

addKeyboardEventHandler :: R.Application -> IO ()
addKeyboardEventHandler
  application@R.Application
    { R.guiObjects =
        R.GuiObjects
          { R.window = window
          }
    }
  =
  void $
    GI.Gtk.onWidgetKeyPressEvent window $
      keyboardEventHandler application

keyboardEventHandler :: R.Application -> GI.Gdk.EventKey -> IO Bool
keyboardEventHandler
  application@R.Application
    { R.guiObjects =
        R.GuiObjects
          { R.volumeButton      = volumeButton
          , R.seekScale         = seekScale
          , R.repeatCheckButton = repeatCheckButton
          }
    , R.ioRefs =
        R.IORefs
          { R.videoInfoRef = videoInfoRef
          }
    }
  eventKey
  = do
  videoInfo       <- readIORef videoInfoRef
  oldVolume       <- GI.Gtk.scaleButtonGetValue volumeButton
  keyValue        <- GI.Gdk.getEventKeyKeyval eventKey
  eventButton     <- GI.Gdk.newZeroEventButton
  rangeValue      <- GI.Gtk.rangeGetValue seekScale
  let isVideo     = R.isVideo videoInfo
  let volumeDelta = 0.05
  -- Mute Toggle
  when (keyValue == GI.Gdk.KEY_m || keyValue == GI.Gdk.KEY_AudioMute) $ do
    let newVolume = if oldVolume <= 0.0 then 0.5 else 0.0
    GI.Gtk.scaleButtonSetValue volumeButton newVolume
  -- Play/Pause Toggle
  when ((keyValue == GI.Gdk.KEY_space || keyValue == GI.Gdk.KEY_AudioPlay) && isVideo) $
    void $ playPauseButtonClickHandler application eventButton
  -- Full Screen Toggle
  when (keyValue == GI.Gdk.KEY_r && isVideo) $ do
    repeatVideo <- GI.Gtk.toggleButtonGetActive repeatCheckButton
    void $ GI.Gtk.toggleButtonSetActive repeatCheckButton (not repeatVideo)
  -- Volume Up
  when (keyValue == GI.Gdk.KEY_Up || keyValue == GI.Gdk.KEY_AudioRaiseVolume) $ do
    let newVolume = if oldVolume >= 1.0 then 1.0 else oldVolume + volumeDelta
    GI.Gtk.scaleButtonSetValue volumeButton newVolume
  -- Volume Down
  when (keyValue == GI.Gdk.KEY_Down || keyValue == GI.Gdk.KEY_AudioLowerVolume) $ do
    let newVolume = if oldVolume <= 0.0 then 0.0 else oldVolume - volumeDelta
    GI.Gtk.scaleButtonSetValue volumeButton newVolume
  -- Seek left
  when (keyValue == GI.Gdk.KEY_Left && isVideo) $
    void $ GI.Gtk.rangeSetValue
      seekScale
      (clamp 0.0 100.0 (rangeValue - keyboardShortcutSeekAdvanceBy))
  -- Seek right
  when (keyValue == GI.Gdk.KEY_Right && isVideo) $
    void $ GI.Gtk.rangeSetValue
      seekScale
      (clamp 0.0 100.0 (rangeValue + keyboardShortcutSeekAdvanceBy))
  -- Show Controls
  when (keyValue == GI.Gdk.KEY_c) $ do
    eventMotion <- GI.Gdk.newZeroEventMotion
    void $ mouseMoveHandler application [fillWindowWithVideo] eventMotion
  -- Full Screen Toggle
  when (keyValue == GI.Gdk.KEY_f && isVideo) $ do
    eventMotion <- GI.Gdk.newZeroEventMotion
    void $ mouseMoveHandler application [fillWindowWithVideo] eventMotion
    void $ fullScreenButtonReleaseHandler application eventButton
  return True

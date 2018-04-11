{-
  Movie Monad
  (C) 2017 David Lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Reset where

import Control.Monad
import Data.Int
import Data.IORef
import Data.GI.Base.Properties
import qualified GI.Gtk
import qualified GI.Gst

import qualified Records as R
import Constants
import Mouse
import VideoInfo
import PlayPause
import Utils

resetApplication :: R.Application -> IO ()
resetApplication
  R.Application
    { R.guiObjects = guiObjects
    , R.playbin    = playbin
    , R.ioRefs     = ioRefs
    }
  = do
  resetIoRefs     ioRefs
  resetPlaybin    playbin
  resetGuiObjects guiObjects

resetIoRefs :: R.IORefs -> IO ()
resetIoRefs
  R.IORefs
    { R.videoInfoRef                           = videoInfoRef
    , R.previousFileNamePathRef                = previousFileNamePathRef
    , R.alteringBottomControlsBoxVisibilityRef = alteringBottomControlsBoxVisibilityRef
    }
  = do
  void $ resetVideoInfo videoInfoRef
  atomicWriteIORef previousFileNamePathRef ""
  atomicWriteIORef alteringBottomControlsBoxVisibilityRef False

resetVideoInfo :: IORef R.VideoInfo -> IO R.VideoInfo
resetVideoInfo videoInfoRef = do
  let videoInfo = R.defaultVideoInfo
  saveVideoInfo videoInfoRef videoInfo
  return videoInfo

resetPlaybin :: GI.Gst.Element -> IO ()
resetPlaybin playbin = do
  void $ GI.Gst.elementSetState playbin GI.Gst.StateNull
  void $ Data.GI.Base.Properties.setObjectPropertyDouble playbin "volume" 0.5
  void $ Data.GI.Base.Properties.setObjectPropertyString playbin "uri" (Just "")

resetGuiObjects :: R.GuiObjects -> IO ()
resetGuiObjects
  R.GuiObjects
    { R.window                           = window
    , R.fileChooserButton                = fileChooserButton
    , R.fileChooserEntry                 = fileChooserEntry
    , R.fileChooserButtonLabel           = fileChooserButtonLabel
    , R.videoWidget                      = videoWidget
    , R.seekScale                        = seekScale
    , R.playPauseButton                  = playPauseButton
    , R.repeatCheckButton                = repeatCheckButton
    , R.windowWidthSelectionComboBoxText = windowWidthSelectionComboBoxText
    , R.videoSpeedSelectionComboboxText  = videoSpeedSelectionComboboxText
    , R.subtitleSelectionComboBoxText    = subtitleSelectionComboBoxText
    , R.fullscreenButton                 = fullscreenButton
    , R.playImage                        = playImage
    , R.pauseImage                       = pauseImage
    , R.bottomControlsBox                = bottomControlsBox
    , R.bufferingSpinner                 = bufferingSpinner
    }
  = do
  desiredWindowWidth <- getDesiredWindowWidth windowWidthSelectionComboBoxText window
  styleContext <- GI.Gtk.widgetGetStyleContext bottomControlsBox
  GI.Gtk.widgetSetSizeRequest window windowMinimumWidth (-1)
  GI.Gtk.windowUnfullscreen window
  GI.Gtk.widgetHide videoWidget
  GI.Gtk.widgetHide bottomControlsBox
  GI.Gtk.widgetHide seekScale
  GI.Gtk.widgetHide playPauseButton
  GI.Gtk.widgetHide repeatCheckButton
  GI.Gtk.widgetHide fullscreenButton
  GI.Gtk.widgetHide subtitleSelectionComboBoxText
  GI.Gtk.widgetHide bufferingSpinner
  GI.Gtk.widgetShow windowWidthSelectionComboBoxText
  GI.Gtk.widgetShow fileChooserButton
  GI.Gtk.comboBoxSetActive subtitleSelectionComboBoxText 0
  GI.Gtk.comboBoxSetActive videoSpeedSelectionComboboxText 1
  GI.Gtk.setToggleButtonActive repeatCheckButton False
  GI.Gtk.entrySetText fileChooserEntry ""
  GI.Gtk.labelSetText fileChooserButtonLabel "Open"
  GI.Gtk.windowResize window (fromIntegral desiredWindowWidth :: Int32) 1
  GI.Gtk.styleContextRemoveClass styleContext fadeInClassName
  GI.Gtk.styleContextRemoveClass styleContext fadeOutClassName
  setCursor window Nothing
  setPlayPauseButton playPauseButton playImage pauseImage False

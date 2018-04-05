{-
  Movie Monad
  (C) 2017 David Lettier
  lettier.com
-}

module Seek where

import Control.Monad
import Data.IORef
import Data.GI.Base.Signals
import GI.GLib
import GI.GObject
import qualified GI.Gtk
import qualified GI.Gst

import qualified Records as R
import Playbin
import Utils

addSeekHandlers :: R.Application -> IO ()
addSeekHandlers
  R.Application
    { R.guiObjects =
        R.GuiObjects
          { R.seekScale = seekScale
          }
    , R.ioRefs =
        R.IORefs
          { R.videoInfoRef = videoInfoRef
          }
    , R.playbin = playbin
    }
  = do
  seekScaleHandlerId <-
    GI.Gtk.onRangeValueChanged seekScale $
      onRangeValueChanged
        playbin
        seekScale
        videoInfoRef
  void $
    GI.GLib.timeoutAdd GI.GLib.PRIORITY_DEFAULT 41 $
      updateSeekScale
        playbin
        seekScale
        seekScaleHandlerId

onRangeValueChanged :: GI.Gst.Element -> GI.Gtk.Scale -> IORef R.VideoInfo -> IO ()
onRangeValueChanged
  playbin
  seekScale
  videoInfoRef
  = do
  maybeDurationAndPosition <- queryPlaybinForDurationAndPosition playbin
  case maybeDurationAndPosition of
    (Just (duration, _)) -> do
      percentage <- clamp 0.0 1.0 . flip (/) 100.0 . clamp 0.0 100.0 <$> GI.Gtk.rangeGetValue seekScale
      let position =
            clamp 0 duration $
                doubleToInt64 $
                  int64toDouble duration * percentage
      void $ GI.Gst.elementSeekSimple playbin GI.Gst.FormatTime [ GI.Gst.SeekFlagsFlush ] position
      videoInfo <- readIORef videoInfoRef
      -- When playing over a network (not local),
      -- sending too many asynchronous seek requests will cause GStreamer to error.
      -- Block until this seek request finishes which will block the main GTK main thread.
      -- Note that by blocking here, the seek scale will not slide.
      -- The user will have to point and click on the scale track.
      unless (R.isLocalFile videoInfo) $
        void $
          GI.Gst.elementGetState playbin GI.Gst.CLOCK_TIME_NONE
    _ -> return ()

updateSeekScale
  ::  GI.Gst.Element
  ->  GI.Gtk.Scale
  ->  Data.GI.Base.Signals.SignalHandlerId
  ->  IO Bool
updateSeekScale
  playbin
  seekScale
  seekScaleHandlerId
  = do
  maybeDurationAndPosition <- queryPlaybinForDurationAndPosition playbin
  case maybeDurationAndPosition of
    (Just (duration, position)) ->
      when (duration > 0) $ do
        let percentage = 100.0 * (int64toDouble position / int64toDouble duration)
        GI.GObject.signalHandlerBlock seekScale seekScaleHandlerId
        GI.Gtk.rangeSetValue seekScale percentage
        GI.GObject.signalHandlerUnblock seekScale seekScaleHandlerId
    _ -> return ()
  return True

hideOrShowSeekScale :: GI.Gtk.Scale -> Bool -> IO ()
hideOrShowSeekScale seekScale True = do
  GI.Gtk.widgetSetSensitive seekScale True
  GI.Gtk.widgetShow seekScale
hideOrShowSeekScale seekScale False = do
  GI.Gtk.widgetSetSensitive seekScale False
  GI.Gtk.widgetHide seekScale

{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

module Seek where

import Control.Monad
import Data.IORef
import Data.Int
import Data.Time.Clock.POSIX
import Data.GI.Base.Signals
import GI.GLib
import GI.GObject
import qualified GI.Gtk
import qualified GI.Gst

import qualified Records as R

addSeekHandlers :: R.Application -> IO ()
addSeekHandlers
  R.Application {
        R.guiObjects = R.GuiObjects {
              R.seekScale = seekScale
          }
      , R.ioRefs = R.IORefs {
              R.videoInfoRef = videoInfoRef
        }
      , R.playbin = playbin
    }
  = do
  seekScaleHandlerId <- GI.Gtk.onRangeValueChanged seekScale (
      onRangeValueChanged
        playbin
        seekScale
        videoInfoRef
    )
  void (
      GI.GLib.timeoutAdd GI.GLib.PRIORITY_DEFAULT 41 (
          updateSeekScale
            playbin
            seekScale
            seekScaleHandlerId
        )
    )

seekRequestLastDeltaThreshold :: R.VideoInfo -> POSIXTime
seekRequestLastDeltaThreshold R.VideoInfo { R.isLocalFile = True }  = 0.0
seekRequestLastDeltaThreshold R.VideoInfo { R.isLocalFile = False } = 0.0

onRangeValueChanged ::
  GI.Gst.Element ->
  GI.Gtk.Scale ->
  IORef R.VideoInfo ->
  IO ()
onRangeValueChanged
  playbin
  seekScale
  videoInfoRef
  = do
  (couldQueryDuration, duration) <- GI.Gst.elementQueryDuration playbin GI.Gst.FormatTime
  when couldQueryDuration $ do
    percentage' <- GI.Gtk.rangeGetValue seekScale
    let percentage = percentage' / 100.0
    let position = fromIntegral (round ((fromIntegral duration :: Double) * percentage) :: Int) :: Int64
    void $ GI.Gst.elementSeekSimple playbin GI.Gst.FormatTime [ GI.Gst.SeekFlagsFlush ] position
    videoInfoGathered <- readIORef videoInfoRef
    -- When playing over a network (not local),
    -- sending too many asynchronous seek requests will cause GStreamer to error.
    -- Block until this seek request finishes which will block the main GTK main thread.
    -- Note that by blocking here, the seek scale will not slide.
    -- The user will have to point and click on the scale track.
    unless (R.isLocalFile videoInfoGathered) $ void $ GI.Gst.elementGetState playbin GI.Gst.CLOCK_TIME_NONE

updateSeekScale ::
  GI.Gst.Element ->
  GI.Gtk.Scale ->
  Data.GI.Base.Signals.SignalHandlerId ->
  IO Bool
updateSeekScale
  playbin
  seekScale
  seekScaleHandlerId
  = do
  (couldQueryDuration, duration) <- GI.Gst.elementQueryDuration playbin GI.Gst.FormatTime
  (couldQueryPosition, position) <- GI.Gst.elementQueryPosition playbin GI.Gst.FormatTime
  when (couldQueryDuration && couldQueryPosition && duration > 0) $ do
    let percentage = 100.0 * (fromIntegral position / fromIntegral duration :: Double)
    GI.GObject.signalHandlerBlock seekScale seekScaleHandlerId
    GI.Gtk.rangeSetValue seekScale percentage
    GI.GObject.signalHandlerUnblock seekScale seekScaleHandlerId
  return True

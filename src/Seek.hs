{-
  Movie Monad
  (C) 2017 David Lettier
  lettier.com
-}

module Seek where

import Control.Monad
import Data.Maybe
import Data.Int
import Data.Word
import Data.IORef
import Data.GI.Base.Signals
import qualified GI.GLib
import qualified GI.GObject
import qualified GI.Gtk
import qualified GI.Gst

import qualified Records as R
import Utils

addSeekHandlers :: R.Application -> IO ()
addSeekHandlers
  application@R.Application
    { R.guiObjects =
        R.GuiObjects
          { R.seekScale = seekScale
          }
    , R.playbin = playbin
    }
  = do
  seekScaleHandlerId <-
    GI.Gtk.onRangeValueChanged seekScale $
      onRangeValueChanged application
  void $
    GI.GLib.timeoutAdd GI.GLib.PRIORITY_DEFAULT 41 $
      updateSeekScale
        playbin
        seekScale
        seekScaleHandlerId

onRangeValueChanged :: R.Application -> IO ()
onRangeValueChanged
  application@R.Application
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
  maybeDurationAndPosition <- queryPlaybinForDurationAndPosition playbin
  case maybeDurationAndPosition of
    (Just (duration, _)) -> do
      percentage <- clamp 0.0 1.0 . flip (/) 100.0 . clamp 0.0 100.0 <$> GI.Gtk.rangeGetValue seekScale
      let position =
            clamp 0 duration $
                doubleToInt64 $
                  int64toDouble duration * percentage
      seekTo application (Just position) Nothing
      -- When playing over a network (not local),
      -- sending too many asynchronous seek requests will cause GStreamer to error.
      -- Block until this seek request finishes which will block the main GTK main thread.
      -- Note that by blocking here, the seek scale will not slide.
      -- The user will have to point and click on the scale track.
      videoInfo <- readIORef videoInfoRef
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

seekTo :: R.Application -> Maybe Int64 -> Maybe Word32 -> IO ()
seekTo
  R.Application
    { R.guiObjects =
        R.GuiObjects
          { R.videoSpeedSelectionComboboxText = videoSpeedSelectionComboboxText
          }
    , R.ioRefs =
        R.IORefs
          { R.videoInfoRef = videoInfoRef
          }
    , R.playbin = playbin
    }
  maybeDesiredPosition
  maybeDelay
  = do
  videoInfo      <- readIORef videoInfoRef
  activeId       <- GI.Gtk.comboBoxGetActive videoSpeedSelectionComboboxText
  let isSeekable = R.isSeekable videoInfo
  let isVideo    = R.isVideo videoInfo
  case (isVideo, isSeekable, activeId) of
    (True, True, 0) -> seekToWithPlaybackRate 0.5
    (True, True, 1) -> seekToWithPlaybackRate 1.0
    (True, True, 2) -> seekToWithPlaybackRate 1.5
    _ -> return ()
  where
    seekToWithPlaybackRate :: Double -> IO ()
    seekToWithPlaybackRate rate =
      case maybeDelay of
        Just delay ->
          void $
            GI.GLib.timeoutAdd
              GI.GLib.PRIORITY_DEFAULT
              (if delay < 0 then 0 else delay) $ do
                createAndSendSeekEvent
                return False
        Nothing -> createAndSendSeekEvent
      where
        createAndSendSeekEvent :: IO ()
        createAndSendSeekEvent = do
          maybeDurationPosition <- queryPlaybinForDurationAndPosition playbin
          case maybeDurationPosition of
            (Just (_, currentPosition)) -> do
              let position = fromMaybe currentPosition maybeDesiredPosition
              let flags =
                      GI.Gst.SeekFlagsFlush
                    : [GI.Gst.SeekFlagsTrickmode | rate /= 1.0]
              void $
                GI.Gst.elementSeek
                  playbin
                  rate
                  GI.Gst.FormatTime
                  flags
                  GI.Gst.SeekTypeSet
                  position
                  GI.Gst.SeekTypeNone
                  0
            Nothing -> return ()

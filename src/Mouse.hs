{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

module Mouse where

import Control.Monad
import Data.Maybe
import Data.Text
import Data.IORef
import Data.Time.Clock.POSIX
import qualified GI.Gdk
import qualified GI.Gtk

import qualified Records as R

addWindowMouseMoveHandlers :: R.Application -> IO ()
addWindowMouseMoveHandlers
  application@R.Application {
        R.guiObjects = R.GuiObjects {
              R.videoWidget = videoWidget
            , R.seekScale = seekScale
          }
    }
  =
  void (GI.Gtk.onWidgetMotionNotifyEvent videoWidget (windowMouseMoveHandler application)) >>
  void (GI.Gtk.onWidgetMotionNotifyEvent seekScale (windowMouseMoveHandler application))

windowMouseMoveHandler ::
  R.Application ->
  GI.Gdk.EventMotion ->
  IO Bool
windowMouseMoveHandler
  R.Application {
        R.guiObjects = R.GuiObjects {
              R.window = window
            , R.fileChooserButton = fileChooserButton
            , R.bottomControlsGtkBox = bottomControlsGtkBox
          }
      , R.ioRefs = R.IORefs {
            R.isWindowFullScreenRef = isWindowFullScreenRef
          , R.mouseMovedLastRef = mouseMovedLastRef
        }
    }
  _
  = do
  isWindowFullScreen <- readIORef isWindowFullScreenRef
  unless isWindowFullScreen $ GI.Gtk.widgetShow fileChooserButton
  GI.Gtk.widgetShow bottomControlsGtkBox
  setCursor window Nothing
  timeNow <- getPOSIXTime
  atomicWriteIORef mouseMovedLastRef (round timeNow)
  return False

setCursor :: GI.Gtk.Window -> Maybe Text -> IO ()
setCursor window Nothing =
  fromJust <$> GI.Gtk.widgetGetWindow window >>=
  flip GI.Gdk.windowSetCursor (Nothing :: Maybe GI.Gdk.Cursor)
setCursor window (Just cursorType) = do
  gdkWindow <- fromJust <$> GI.Gtk.widgetGetWindow window
  maybeCursor <- makeCursor gdkWindow cursorType
  GI.Gdk.windowSetCursor gdkWindow maybeCursor

makeCursor :: GI.Gdk.Window -> Text -> IO (Maybe GI.Gdk.Cursor)
makeCursor window cursorType =
  GI.Gdk.windowGetDisplay window >>=
  flip GI.Gdk.cursorNewFromName cursorType

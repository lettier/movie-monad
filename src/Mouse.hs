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

addMouseMoveHandlers :: R.Application -> [R.Application -> IO ()] -> IO ()
addMouseMoveHandlers
  application@R.Application {
        R.guiObjects = R.GuiObjects {
              R.videoWidget = videoWidget
            , R.seekScale = seekScale
          }
    }
  onMouseMoveCallbacks
  =
      void (GI.Gtk.onWidgetMotionNotifyEvent videoWidget mouseMoveHandler')
  >>  void (GI.Gtk.onWidgetMotionNotifyEvent seekScale   mouseMoveHandler')
  where
    mouseMoveHandler' :: GI.Gdk.EventMotion -> IO Bool
    mouseMoveHandler' = mouseMoveHandler application onMouseMoveCallbacks

mouseMoveHandler
  :: R.Application
  -> [R.Application -> IO ()]
  -> GI.Gdk.EventMotion
  -> IO Bool
mouseMoveHandler
  application@R.Application {
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
  onMouseMoveCallbacks
  _
  = do
  isWindowFullScreen <- readIORef isWindowFullScreenRef
  unless isWindowFullScreen $ GI.Gtk.widgetShow fileChooserButton
  GI.Gtk.widgetShow bottomControlsGtkBox
  setCursor window Nothing
  timeNow <- getPOSIXTime
  atomicWriteIORef mouseMovedLastRef (round timeNow)
  mapM_ (\ f -> f application) onMouseMoveCallbacks
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

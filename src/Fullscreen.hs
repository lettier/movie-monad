{-
  Movie Monad
  (C) 2017 David Lettier
  lettier.com
-}

module Fullscreen where

import Control.Monad
import Data.IORef
import qualified GI.Gdk
import qualified GI.Gtk

import qualified Records as R

addFullscreenButtonReleaseHandler :: R.Application -> IO ()
addFullscreenButtonReleaseHandler
  application@R.Application
    { R.guiObjects =
        R.GuiObjects
          { R.fullscreenButton = fullscreenButton
          }
    }
  =
  void $
    GI.Gtk.onWidgetButtonReleaseEvent
      fullscreenButton $
        fullscreenButtonReleaseHandler application

fullscreenButtonReleaseHandler :: R.Application -> GI.Gdk.EventButton -> IO Bool
fullscreenButtonReleaseHandler
  R.Application
    { R.guiObjects =
        R.GuiObjects
          { R.window = window
          , R.fileChooserButton                = fileChooserButton
          , R.windowWidthSelectionComboBoxText = windowWidthSelectionComboBoxText
          }
    , R.ioRefs =
        R.IORefs
          { R.isWindowFullScreenRef = isWindowFullScreenRef
          }
    }
  _
  = do
  isWindowFullScreen <- readIORef isWindowFullScreenRef
  if isWindowFullScreen
    then do
      GI.Gtk.widgetShow windowWidthSelectionComboBoxText
      GI.Gtk.widgetShow fileChooserButton
      void $ GI.Gtk.windowUnfullscreen window
    else do
      GI.Gtk.widgetHide windowWidthSelectionComboBoxText
      GI.Gtk.widgetHide fileChooserButton
      void $ GI.Gtk.windowFullscreen window
  return True

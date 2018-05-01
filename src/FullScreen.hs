{-
  Movie Monad
  (C) 2017 David Lettier
  lettier.com
-}

module FullScreen where

import Control.Monad
import Data.IORef
import qualified GI.Gdk
import qualified GI.Gtk

import qualified Records as R

addFullScreenButtonReleaseHandler :: R.Application -> IO ()
addFullScreenButtonReleaseHandler
  application@R.Application
    { R.guiObjects =
        R.GuiObjects
          { R.fullScreenButton = fullScreenButton
          }
    }
  =
  void $
    GI.Gtk.onWidgetButtonReleaseEvent
      fullScreenButton $
        fullScreenButtonReleaseHandler application

fullScreenButtonReleaseHandler :: R.Application -> GI.Gdk.EventButton -> IO Bool
fullScreenButtonReleaseHandler
  R.Application
    { R.guiObjects =
        R.GuiObjects
          { R.window                           = window
          , R.fileChooserDialogButton          = fileChooserDialogButton
          , R.windowWidthSelectionComboBoxText = windowWidthSelectionComboBoxText
          , R.topControlsBox                   = topControlsBox
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
      GI.Gtk.widgetShow fileChooserDialogButton
      GI.Gtk.widgetShow topControlsBox
      void $ GI.Gtk.windowUnfullscreen window
    else do
      GI.Gtk.widgetHide windowWidthSelectionComboBoxText
      GI.Gtk.widgetHide fileChooserDialogButton
      GI.Gtk.widgetHide topControlsBox
      void $ GI.Gtk.windowFullscreen window
  return True

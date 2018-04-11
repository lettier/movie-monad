{-
  Movie Monad
  (C) 2017 David Lettier
  lettier.com
-}

module WindowWidthSelector where

import Control.Monad
import Data.Text
import Data.IORef
import qualified GI.GLib
import qualified GI.Gtk

import qualified Records as R
import Reset
import Window
import Utils

addWindowWidthSelectorHandlers :: R.Application -> IO ()
addWindowWidthSelectorHandlers
  application@R.Application
    { R.guiObjects =
        guiObjects@R.GuiObjects
          { R.windowWidthSelectionComboBoxText = windowWidthSelectionComboBoxText
          }
    }
  = do
  void $
    GI.Gtk.onComboBoxChanged
      windowWidthSelectionComboBoxText $
        videoSizeSelectionHandler application
  void $
    GI.GLib.timeoutAddSeconds
      GI.GLib.PRIORITY_DEFAULT
      1 $
        setToCustomIfNeeded guiObjects

videoSizeSelectionHandler :: R.Application -> IO ()
videoSizeSelectionHandler
  application@R.Application
    { R.guiObjects =
        guiObjects@R.GuiObjects
          { R.window                           = window
          , R.windowWidthSelectionComboBoxText = windowWidthSelectionComboBoxText
          }
    , R.ioRefs =
        R.IORefs
          { R.videoInfoRef = videoInfoRef
          }
    }
  = do
  videoInfo         <- readIORef videoInfoRef
  desiredWindowWidth <- getDesiredWindowWidth windowWidthSelectionComboBoxText window
  maybeWindowSize   <- calculateWindowSize guiObjects desiredWindowWidth videoInfo
  case maybeWindowSize of
    Nothing              -> resetApplication application
    Just (width, height) -> setWindowSize guiObjects width height

setToCustomIfNeeded :: R.GuiObjects -> IO Bool
setToCustomIfNeeded
  R.GuiObjects
    { R.window                      = window
    , R.windowWidthSelectionComboBoxText = windowWidthSelectionComboBoxText
    }
  = do
  (windowWidth', _)  <- GI.Gtk.windowGetSize window
  maybeSelectedWidth <- getSelectedWindowWidth windowWidthSelectionComboBoxText
  let windowWidth    = fromIntegral windowWidth' :: Int
  case maybeSelectedWidth of
    Just selectedWidth ->
      when (windowWidth /= selectedWidth) $
        GI.Gtk.comboBoxSetActive windowWidthSelectionComboBoxText (-1)
    Nothing -> do
      let windowWidthText = Data.Text.pack $ show windowWidth
      present <-
        GI.Gtk.comboBoxSetActiveId
          windowWidthSelectionComboBoxText $
            Just windowWidthText
      if present
        then return ()
        else GI.Gtk.comboBoxSetActive windowWidthSelectionComboBoxText (-1)
  return True

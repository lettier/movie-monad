{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

module VideoSizeSelector where

import Control.Monad
import Data.Text
import qualified GI.Gtk

import qualified Records as R
import Window
import VideoInfo
import Utils

addVideoSizeSelectorHandler :: R.Application -> IO ()
addVideoSizeSelectorHandler
  application@R.Application {
        R.guiObjects = R.GuiObjects {
              R.videoWidthSelectionComboBox = videoWidthSelectionComboBox
          }
    }
  = void (
        GI.Gtk.onComboBoxChanged
          videoWidthSelectionComboBox
          (videoSizeSelectionHandler application)
      )

videoSizeSelectionHandler ::
  R.Application ->
  IO ()
videoSizeSelectionHandler
  R.Application {
        R.guiObjects = guiObjects@R.GuiObjects {
              R.videoWidthSelectionComboBox = videoWidthSelectionComboBox
            , R.fileChooserEntry = fileChooserEntry
          }
      , R.ioRefs = R.IORefs {
              R.videoInfoRef = videoInfoRef
        }
    }
  = do
  filePathName <- Data.Text.unpack <$> GI.Gtk.entryGetText fileChooserEntry
  videoWidthSelection <- getSelectedVideoWidth videoWidthSelectionComboBox
  retrievedVideoInfo <- getVideoInfo videoInfoRef filePathName
  maybeWindowSize <- calculateWindowSize guiObjects videoWidthSelection retrievedVideoInfo
  case maybeWindowSize of
    Nothing -> resetWindow guiObjects
    Just (width, height) -> setWindowSize guiObjects width height

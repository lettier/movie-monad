{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

module SubtitleSelector where

import Control.Monad
import Data.Text
import qualified GI.Gtk

import qualified Records as R
import Playbin

addSubtitleSelectorHandler :: R.Application -> IO ()
addSubtitleSelectorHandler
  application@R.Application {
        R.guiObjects = R.GuiObjects {
              R.subtitleSelectionComboBox = subtitleSelectionComboBox
          }
    }
  = void (
        GI.Gtk.onComboBoxChanged
          subtitleSelectionComboBox
          (subtitleSelectorHandler application)
      )

subtitleSelectorHandler :: R.Application -> IO ()
subtitleSelectorHandler
  R.Application {
        R.guiObjects = R.GuiObjects {
              R.subtitleSelectionComboBox = subtitleSelectionComboBox
          }
      , R.playbin = playbin
    }
  = do
    maybeActiveId <- GI.Gtk.getComboBoxActiveId subtitleSelectionComboBox
    nText <- getTextStreamCount playbin
    case maybeActiveId of
      Nothing -> return ()
      Just activeId -> do
        let activeId' = read (Data.Text.unpack activeId) :: Int
        if activeId' == (-1)
          then turnOffSubtitles playbin
          else when (activeId' >= 0 && activeId' < nText) $ do
            setCurrentTextStreamId playbin activeId'
            turnOnSubtitles playbin

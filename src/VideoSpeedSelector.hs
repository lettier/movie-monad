{-
  Movie Monad
  (C) 2018 David Lettier
  lettier.com
-}

module VideoSpeedSelector where

import Control.Monad
import qualified GI.Gtk

import qualified Records as R
import Seek

data PlaybackRate = PlaybackRateSlow | PlaybackRateNormal | PlaybackRateFast

addVideoSpeedSelectionHandlers :: R.Application -> IO ()
addVideoSpeedSelectionHandlers
  application@R.Application
    { R.guiObjects =
        R.GuiObjects
          { R.videoSpeedSelectionComboboxText = videoSpeedSelectionComboboxText
          }
    }
  =
  void $
    GI.Gtk.onComboBoxChanged
      videoSpeedSelectionComboboxText $
        seekTo application Nothing Nothing

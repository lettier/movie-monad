{-
  Movie Monad
  (C) 2017 David Lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Constants where

import Data.Text

hideOnScreenControlsInterval :: Integral a => a
hideOnScreenControlsInterval = 7

invalidVideoWidgetName :: Data.Text.Text
invalidVideoWidgetName = "invalid-video-widget"

keyboardShortcutSeekAdvanceBy :: Double
keyboardShortcutSeekAdvanceBy = 1.0

windowMinimumSize :: Integral a => a
windowMinimumSize = 448

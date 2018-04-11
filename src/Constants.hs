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

windowMinimumWidth :: Integral a => a
windowMinimumWidth = 480

fadeInClassName :: Text
fadeInClassName = "movie-monad-fade-in"

fadeOutClassName :: Text
fadeOutClassName = "movie-monad-fade-out"

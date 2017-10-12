{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Constants where

import Data.Text

hideOnScreenControlsInterval :: Integral a => a
hideOnScreenControlsInterval = 5

invalidVideoWidgetName :: Data.Text.Text
invalidVideoWidgetName = "invalid-video-widget"

{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

module MovieMonadLib where

data VideoInfo = VideoInfo {
      uri         :: Prelude.String
    , isLocalFile :: Bool
    , isVideo     :: Bool
    , isSeekable  :: Bool
    , videoWidth  :: Int
    , videoHeight :: Int
  }

defaultVideoInfo :: VideoInfo
defaultVideoInfo  = VideoInfo {
      uri         = ""
    , isLocalFile = False
    , isVideo     = False
    , isSeekable  = False
    , videoWidth  = 800
    , videoHeight = 600
  }

{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VideoInfo where

import System.Exit
import Text.Read
import Data.Maybe
import Data.Text
import Data.IORef

import qualified Records as R
import Uri
import Utils

getVideoInfoRaw :: Prelude.String -> IO (Maybe Prelude.String)
getVideoInfoRaw uri = do
  (code, out, _) <- safeRunProcessGetOutput "gst-discoverer-1.0" [uri, "-v"]
  if code == System.Exit.ExitSuccess
    then return (Just out)
    else return Nothing

getVideoInfo :: IORef R.VideoInfo -> Prelude.String -> IO R.VideoInfo
getVideoInfo videoInfoRef filePathName = do
  videoInfoGathered <- readIORef videoInfoRef
  uri <- addUriSchemeIfNone filePathName
  cacheDo (R.uri videoInfoGathered == uri) videoInfoGathered uri
  where
    widthField :: Data.Text.Text
    widthField = "width: "
    heightField :: Data.Text.Text
    heightField = "height: "
    seekableField :: Data.Text.Text
    seekableField = "seekable: "
    cacheDo :: Bool -> R.VideoInfo -> Prelude.String -> IO R.VideoInfo
    cacheDo True  videoInfoGathered _   = return videoInfoGathered
    cacheDo False _                 uri = do
      videoInfoRaw <- getVideoInfoRaw uri
      processVideoInfoRaw videoInfoRaw uri
    processVideoInfoRaw :: Maybe Prelude.String -> Prelude.String -> IO R.VideoInfo
    processVideoInfoRaw Nothing _ = do
      let videoInfoGathered = R.defaultVideoInfo
      atomicWriteIORef videoInfoRef videoInfoGathered
      return videoInfoGathered
    processVideoInfoRaw (Just str) uri = do
      let text      = Data.Text.toLower $ Data.Text.pack str
      let textLines = Data.Text.lines text
      let videoInfoGathered = R.VideoInfo {
              R.uri          = uri
            , R.isLocalFile  = hasFileUriScheme uri
            , R.isVideo      = "video: video/" `Data.Text.isInfixOf` text
            , R.isSeekable   = "yes" == getField seekableField textLines
            , R.videoWidth   = getDimension R.videoWidth  widthField textLines
            , R.videoHeight  = getDimension R.videoHeight heightField textLines
          }
      atomicWriteIORef videoInfoRef videoInfoGathered
      return videoInfoGathered
    getDimension :: (R.VideoInfo -> Int) -> Data.Text.Text -> [Data.Text.Text] -> Int
    getDimension f field lines' = fromMaybe (
        f R.defaultVideoInfo
      ) (readMaybe (Data.Text.unpack $ getField field lines') :: Maybe Int)
    getField :: Data.Text.Text -> [Data.Text.Text] -> Data.Text.Text
    getField field =
      Prelude.foldl (\ acc l ->
          if not $ Data.Text.null acc
            then acc
            else if field `Data.Text.isInfixOf` l
              then Data.Text.replace field "" $ Data.Text.strip l
              else ""
        ) ""

{-
  Movie Monad
  (C) 2017 David Lettier
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

getRawVideoInfo :: Prelude.String -> IO (Maybe Prelude.String)
getRawVideoInfo uri = do
  (code, out, _) <- safeRunProcessGetOutput "gst-discoverer-1.0" [uri, "-v"]
  if code == System.Exit.ExitSuccess
    then return (Just out)
    else return Nothing

processRawVideoInfo :: Maybe Prelude.String -> Prelude.String -> Maybe R.VideoInfo
processRawVideoInfo Nothing _ = Nothing
processRawVideoInfo (Just str) uri = do
  let text      = Data.Text.toLower $ Data.Text.pack str
  let textLines = Data.Text.lines text
  let videoInfo =
        R.VideoInfo
          { R.uri          = uri
          , R.isLocalFile  = hasFileUriScheme uri
          , R.isVideo      = "video: video/" `Data.Text.isInfixOf` text
          , R.isSeekable   = "yes" == getField seekableField textLines
          , R.videoWidth   = getDimension R.videoWidth  widthField textLines
          , R.videoHeight  = getDimension R.videoHeight heightField textLines
          }
  Just videoInfo
  where
    widthField    :: Data.Text.Text
    widthField    = "width: "
    heightField   :: Data.Text.Text
    heightField   = "height: "
    seekableField :: Data.Text.Text
    seekableField = "seekable: "
    getDimension :: (R.VideoInfo -> Int) -> Data.Text.Text -> [Data.Text.Text] -> Int
    getDimension f field lines' =
      fromMaybe
        (f R.defaultVideoInfo)
        (readMaybe (Data.Text.unpack $ getField field lines') :: Maybe Int)
    getField :: Data.Text.Text -> [Data.Text.Text] -> Data.Text.Text
    getField field =
      Prelude.foldl
        (\ acc l ->
          if not $ Data.Text.null acc
            then acc
            else
              if field `Data.Text.isInfixOf` l
                then Data.Text.replace field "" $ Data.Text.strip l
                else ""
        )
      ""
getVideoInfo :: IORef R.VideoInfo -> Prelude.String -> IO (Maybe R.VideoInfo)
getVideoInfo videoInfoRef filePathName = do
  videoInfo <- readIORef videoInfoRef
  uri       <- addUriSchemeIfNone filePathName
  if R.uri videoInfo == uri
    then return $ Just videoInfo
    else do
      rawVideoInfo <- getRawVideoInfo uri
      case processRawVideoInfo rawVideoInfo uri of
        (Just videoInfo') -> return $ Just videoInfo'
        _ -> return Nothing

saveVideoInfo :: IORef R.VideoInfo -> R.VideoInfo -> IO ()
saveVideoInfo = atomicWriteIORef


{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Uri where

import System.FilePath
import Filesystem
import Filesystem.Path.CurrentOS
import Data.Text

removeFileUriScheme :: Prelude.String -> Prelude.String
removeFileUriScheme str =
  if hasFileUriScheme str
    then Data.Text.unpack $ Data.Text.replace "file://" "" uri
    else Data.Text.unpack uri
  where
    uri :: Data.Text.Text
    uri = Data.Text.pack str

hasFileUriScheme :: Prelude.String -> Bool
hasFileUriScheme = hasUriScheme "file://"

hasHttpUriScheme :: Prelude.String -> Bool
hasHttpUriScheme str = isHttp || isHttps
  where
    isHttp :: Bool
    isHttp  = hasUriScheme "http://"  str
    isHttps :: Bool
    isHttps = hasUriScheme "https://" str

hasUriScheme :: Prelude.String -> Prelude.String -> Bool
hasUriScheme scheme uri = toLower' scheme `Data.Text.isPrefixOf` toLower' uri
  where
    toLower' :: Prelude.String -> Text
    toLower' = Data.Text.toLower . Data.Text.pack

addUriSchemeIfNone :: Prelude.String -> IO Prelude.String
addUriSchemeIfNone filePathName =
  isLocalFile filePathName >>=
  \ local ->
    return $
      if local
        then if hasFileUriScheme filePathName then filePathName else "file://" ++ filePathName
        else if hasHttpUriScheme filePathName then filePathName else "http://" ++ filePathName

isLocalFile :: Prelude.String -> IO Bool
isLocalFile = Filesystem.isFile . Filesystem.Path.CurrentOS.fromText . Data.Text.pack . removeFileUriScheme

toAbsoluteUri :: String -> IO String
toAbsoluteUri uri = do
  cwd <- getWorkingDirectory
  let cwd' = case toText cwd of
              Right x -> x
              Left  y -> y
  isLocal <- isLocalFile uri
  if not isLocal
    then return uri
    else
      if isRelative uri
        then return $ normalise $ combine (Data.Text.unpack cwd') uri
        else return uri

{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Paths_movie_monad where

dataDir :: String
dataDir = "./src/"

getDataFileName :: FilePath -> IO FilePath
getDataFileName a = do
  putStrLn "You are using a fake Paths_movie_monad."
  return (dataDir ++ "/" ++ a)

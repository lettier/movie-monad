{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import System.Exit
import System.Process
import System.FilePath
import Control.Monad
import Control.Exception
import Data.List
import Data.Text
import Data.Int
import qualified GI.Gtk

fileNameFromFilePathName :: Data.Text.Text -> Data.Text.Text
fileNameFromFilePathName = Data.Text.pack . System.FilePath.takeFileName . Data.Text.unpack

enumToInt32 :: (Enum a, Ord a) => a -> Int32
enumToInt32 enum = fromIntegral (fromEnum enum) :: Int32

isTextEmpty :: Data.Text.Text -> Bool
isTextEmpty = Data.Text.null . Data.Text.strip

getSelectedVideoWidth :: GI.Gtk.ComboBoxText -> IO Int
getSelectedVideoWidth = fmap (\ x -> read (Data.Text.unpack x) :: Int) . GI.Gtk.comboBoxTextGetActiveText

linuxProcessIsRunning :: String -> IO Bool
linuxProcessIsRunning processName = do
  (_, out, _) <- safeRunProcessGetOutput "pgrep" ["-f", processName]
  return (not $ Data.List.null out)

spawnLinuxProcessAndDisown :: String -> IO ()
spawnLinuxProcessAndDisown processCommand =
  void $
    System.Process.spawnCommand
      (
              "nohup "
          ++  processCommand
          ++  " </dev/null &>/dev/null & disown"
        )

safeRunProcessGetOutput :: String -> [String] -> IO (System.Exit.ExitCode, String, String)
safeRunProcessGetOutput processName args =
  catch (
      readProcessWithExitCode
        processName
        args
        ""
    ) (\ (e :: Control.Exception.IOException) ->
        print e >>
        return (ExitFailure 1, "", "")
      )

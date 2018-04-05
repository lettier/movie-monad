{-
  Movie Monad
  (C) 2017 David Lettier
  lettier.com
-}

module Utils where

import System.Exit
import System.Process
import System.FilePath
import Control.Monad
import Control.Exception
import Text.Read
import Data.List
import Data.Text
import Data.Int
import qualified GI.Gtk

fileNameFromFilePathName :: Data.Text.Text -> Data.Text.Text
fileNameFromFilePathName = Data.Text.pack . System.FilePath.takeFileName . Data.Text.unpack

enumToInt32 :: (Enum a, Ord a) => a -> Int32
enumToInt32 enum = fromIntegral (fromEnum enum) :: Int32

int64toDouble :: Int64 -> Double
int64toDouble i = fromIntegral i :: Double

doubleToInt64 :: Double -> Int64
doubleToInt64 d = fromIntegral (round d :: Int) :: Int64

isTextEmpty :: Data.Text.Text -> Bool
isTextEmpty = Data.Text.null . Data.Text.strip

getDesiredWindowWidth :: GI.Gtk.ComboBoxText -> GI.Gtk.Window -> IO Int
getDesiredWindowWidth windowWidthSelectionComboBoxText window = do
  maybeSelectedWindowWidth <- getSelectedWindowWidth windowWidthSelectionComboBoxText
  case maybeSelectedWindowWidth of
    Just selectedWidth -> return selectedWidth
    _                  -> do
      (windowWidth, _) <- GI.Gtk.windowGetSize window
      return (fromIntegral windowWidth :: Int)

getSelectedWindowWidth :: GI.Gtk.ComboBoxText -> IO (Maybe Int)
getSelectedWindowWidth windowWidthSelectionComboBoxText = do
  activeId <- GI.Gtk.comboBoxGetActive windowWidthSelectionComboBoxText
  if activeId == (-1)
    then return Nothing
    else do
      string <-
        Data.Text.unpack <$>
          GI.Gtk.comboBoxTextGetActiveText
            windowWidthSelectionComboBoxText
      return (readMaybe string :: Maybe Int)

linuxProcessIsRunning :: String -> IO Bool
linuxProcessIsRunning processName = do
  (_, out, _) <- safeRunProcessGetOutput "pgrep" ["-f", processName]
  return (not $ Data.List.null out)

spawnLinuxProcessAndDisown :: String -> IO ()
spawnLinuxProcessAndDisown processCommand =
  void $
    System.Process.spawnCommand
      (  "nohup "
      ++ processCommand
      ++ " </dev/null &>/dev/null & disown"
      )

safeRunProcessGetOutput :: String -> [String] -> IO (System.Exit.ExitCode, String, String)
safeRunProcessGetOutput processName args =
  catch readProcess' catchError
  where
    readProcess' :: IO (System.Exit.ExitCode, String, String)
    readProcess' =
      readProcessWithExitCode
        processName
        args
        ""
    catchError :: Control.Exception.IOException -> IO (System.Exit.ExitCode, String, String)
    catchError e = do
      putStr $ show e
      return (ExitFailure 1, "", "")

clamp :: Ord a => a -> a -> a -> a
clamp minimum' maximum' el
  | el < minimum' = minimum'
  | el > maximum' = maximum'
  | otherwise     = el

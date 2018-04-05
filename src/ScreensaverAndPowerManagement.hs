{-
  Movie Monad
  (C) 2017 David Lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ScreensaverAndPowerManagement where

import System.Process
import Control.Monad
import Data.Text

import qualified Records as R (ScreensaverAndPowerManagementActions(..))
import Utils (safeRunProcessGetOutput, linuxProcessIsRunning, spawnLinuxProcessAndDisown)

disable :: String -> IO R.ScreensaverAndPowerManagementActions
disable "linux" = do
  disabledPowerManagement <- disablePowerManagement
  disabledXScreensaver <- disableXScreensaver
  disabledCinnamonScreensaver <- disableCinnamonScreensaver
  disabledGnomeScreensaver <- disableGnomeScreensaver
  return R.ScreensaverAndPowerManagementActions {
        R.disabledPowerManagement = disabledPowerManagement
      , R.disabledXScreensaver = disabledXScreensaver
      , R.disabledCinnamonScreensaver = disabledCinnamonScreensaver
      , R.disabledGnomeScreensaver = disabledGnomeScreensaver
    }
disable _ =
  return R.ScreensaverAndPowerManagementActions {
        R.disabledPowerManagement = False
      , R.disabledXScreensaver = False
      , R.disabledCinnamonScreensaver = False
      , R.disabledGnomeScreensaver = False
    }

enable :: String -> R.ScreensaverAndPowerManagementActions -> IO ()
enable
  "linux"
  R.ScreensaverAndPowerManagementActions {
        R.disabledPowerManagement = disabledPowerManagement
      , R.disabledXScreensaver = disabledXScreensaver
      , R.disabledCinnamonScreensaver = disabledCinnamonScreensaver
      , R.disabledGnomeScreensaver = disabledGnomeScreensaver
    }
  = do
  when disabledPowerManagement enablePowerManagement
  when disabledXScreensaver enableXScreensaver
  when disabledCinnamonScreensaver enableCinnamonScreensaver
  when disabledGnomeScreensaver enableGnomeScreensaver
  return ()
enable _ _ = return ()

disablePowerManagement :: IO Bool
disablePowerManagement = do
  (_, out, _) <- safeRunProcessGetOutput "xset" ["-q"]
  let isEnabled =
        Data.Text.isInfixOf "dpms is enabled" $
          Data.Text.toLower $
            Data.Text.pack out
  if isEnabled
    then do
      _ <- System.Process.spawnCommand "sleep 1; xset -dpms s off"
      return True
    else return False

enablePowerManagement :: IO ()
enablePowerManagement =
  void $
    System.Process.spawnCommand "sleep 1; xset +dpms s on"

disableXScreensaver :: IO Bool
disableXScreensaver =
  disableScreensaver
    "xscreensaver"
    "xscreensaver-command --exit"

enableXScreensaver :: IO ()
enableXScreensaver = spawnLinuxProcessAndDisown "xscreensaver -no-splash"

disableCinnamonScreensaver :: IO Bool
disableCinnamonScreensaver =
  disableScreensaver
    "cinnamon-screensaver"
    "cinnamon-screensaver-command --exit"

enableCinnamonScreensaver :: IO ()
enableCinnamonScreensaver = spawnLinuxProcessAndDisown "cinnamon-screensaver"

disableGnomeScreensaver :: IO Bool
disableGnomeScreensaver =
  disableScreensaver
    "gnome-screensaver"
    "gnome-screensaver-command --exit"

enableGnomeScreensaver :: IO ()
enableGnomeScreensaver = spawnLinuxProcessAndDisown "gnome-screensaver"

disableScreensaver :: String -> String -> IO Bool
disableScreensaver processName disableCommand = do
  isRunning <- linuxProcessIsRunning processName
  if isRunning
    then do
      spawnLinuxProcessAndDisown disableCommand
      return True
    else return False

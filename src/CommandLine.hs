{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

module CommandLine where

import System.Environment
import Data.Text
import GI.Gtk

import qualified Records as R
import FileChooser
import Uri
import Utils

playVideoFromCommandLineIfNeeded :: R.Application -> IO ()
playVideoFromCommandLineIfNeeded
  application@R.Application {
        R.guiObjects = R.GuiObjects {
              R.fileChooserEntry = fileChooserEntry
          }
    }
  = do
  args <- getArgs
  case args of
    [] -> return ()
    (filePath':_) -> do
      filePath <- toAbsoluteUri filePath'
      GI.Gtk.entrySetText fileChooserEntry (Data.Text.pack filePath)
      fileChooserDialogResponseHandler application (enumToInt32 GI.Gtk.ResponseTypeOk)

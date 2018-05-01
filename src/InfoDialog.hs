{-
  Movie Monad
  (C) 2017 David Lettier
  lettier.com
-}

module InfoDialog where

import Control.Monad
import qualified GI.Gdk
import qualified GI.Gtk

import qualified Records as R

addInfoDialogHandler :: R.Application -> IO ()
addInfoDialogHandler
  R.Application
    { R.guiObjects =
        R.GuiObjects
          { R.infoDialogButton = infoDialogButton
          , R.infoDialog       = infoDialog
          }
    }
  =
  void $
    GI.Gtk.onWidgetButtonReleaseEvent
      infoDialogButton $
        infoDialogButtonReleaseHandler infoDialog

infoDialogButtonReleaseHandler :: GI.Gtk.AboutDialog -> GI.Gdk.EventButton -> IO Bool
infoDialogButtonReleaseHandler infoDialog _ = do
  _ <- GI.Gtk.onDialogResponse infoDialog (\ _ -> GI.Gtk.widgetHide infoDialog)
  _ <- GI.Gtk.dialogRun infoDialog
  return True

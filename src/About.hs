{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

module About where

import Control.Monad
import qualified GI.Gdk
import qualified GI.Gtk

import qualified Records as R

addAboutHandler :: R.Application -> IO ()
addAboutHandler
  R.Application {
        R.guiObjects = R.GuiObjects {
              R.aboutButton = aboutButton
            , R.aboutDialog = aboutDialog
          }
    }
  =
  void (
      GI.Gtk.onWidgetButtonReleaseEvent aboutButton (aboutButtonReleaseHandler aboutDialog)
    )

aboutButtonReleaseHandler ::
  GI.Gtk.AboutDialog ->
  GI.Gdk.EventButton ->
  IO Bool
aboutButtonReleaseHandler aboutDialog _ =
  void (
      GI.Gtk.onDialogResponse aboutDialog (\ _ -> GI.Gtk.widgetHide aboutDialog)
    ) >>
  void (
      GI.Gtk.dialogRun aboutDialog
    ) >>
  return True

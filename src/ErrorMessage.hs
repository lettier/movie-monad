{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}

module ErrorMessage where

import Control.Monad
import Data.Text
import Data.Int
import qualified GI.Gtk

import qualified Records as R

addErrorMessageDialogHandler :: R.Application -> IO ()
addErrorMessageDialogHandler
  R.Application {
        R.guiObjects = R.GuiObjects {
              R.errorMessageDialog = errorMessageDialog
          }
    }
  =
  void (GI.Gtk.onDialogResponse errorMessageDialog (errorMessageDialogResponseHandler errorMessageDialog))

errorMessageDialogResponseHandler ::
  GI.Gtk.MessageDialog ->
  Int32 ->
  IO ()
errorMessageDialogResponseHandler errorMessageDialog _ =
  GI.Gtk.widgetHide errorMessageDialog >>
  GI.Gtk.setMessageDialogText errorMessageDialog "There was an error."

runErrorMessageDialog ::
  GI.Gtk.MessageDialog ->
  Text ->
  IO ()
runErrorMessageDialog errorMessageDialog text =
  GI.Gtk.setMessageDialogText errorMessageDialog text >>
  void (GI.Gtk.dialogRun errorMessageDialog)

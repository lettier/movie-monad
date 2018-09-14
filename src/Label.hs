{-
  Movie Monad
  (C) 2018 David Lettier
  lettier.com
-}

{-# LANGUAGE
    NamedFieldPuns
#-}

module Label where

import Control.Monad
import Data.Text
import qualified GI.Gtk

import Utils

import qualified Records as R

addLabelHandlers
  ::  R.Application
  ->  IO ()
addLabelHandlers
  R.Application
    { R.guiObjects =
        R.GuiObjects
          { R.infoDialogLabel
          }
    }
  =
  handleLabelClick infoDialogLabel

handleLabelClick
  ::  GI.Gtk.Label
  ->  IO ()
handleLabelClick
  label
  =
  void $
    GI.Gtk.onLabelActivateLink
      label $
        \ uri -> do
          openUriWithDefaultProgram
            (Data.Text.unpack uri)
          return True

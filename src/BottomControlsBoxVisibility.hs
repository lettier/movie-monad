{-
  Movie Monad
  (C) 2018 David Lettier
  lettier.com
-}

module BottomControlsBoxVisibility
  ( fadeInBottomControlsBox
  , fadeOutBottomControlsBox
  )
  where

import Data.Word
import Data.IORef
import qualified GI.Gtk
import qualified GI.GLib

import qualified Records as R
import Constants

fadeInBottomControlsBox :: R.Application -> Int -> IO (Maybe Word32)
fadeInBottomControlsBox
  application@R.Application
    { R.guiObjects =
        R.GuiObjects
          { R.bottomControlsBox = bottomControlsBox
          }
    , R.ioRefs =
        R.IORefs
          { R.alteringBottomControlsBoxVisibilityRef =
              alteringBottomControlsBoxVisibilityRef
          }
    }
  count
  = do
  visible   <- GI.Gtk.widgetGetVisible bottomControlsBox
  altering  <- readIORef alteringBottomControlsBoxVisibilityRef
  let force = count > 2
  case (visible, force, altering) of
    (True, False, False) -> return Nothing
    (_, True, _) -> do
      atomicWriteIORef alteringBottomControlsBoxVisibilityRef True
      styleContext <- GI.Gtk.widgetGetStyleContext bottomControlsBox
      GI.Gtk.widgetShow bottomControlsBox
      resetBottomControlsAlteringVisibilityState
        styleContext
        alteringBottomControlsBoxVisibilityRef
      return Nothing
    (_, False, True) -> do
      idd <- GI.GLib.timeoutAddSeconds
        GI.GLib.PRIORITY_DEFAULT
        1 $ do
          _ <- fadeInBottomControlsBox application $ count + 1
          return False
      return $ Just idd
    (False, False, False) -> do
      atomicWriteIORef alteringBottomControlsBoxVisibilityRef True
      GI.Gtk.widgetShow bottomControlsBox
      styleContext <- GI.Gtk.widgetGetStyleContext bottomControlsBox
      GI.Gtk.styleContextAddClass styleContext fadeInClassName
      idd <- GI.GLib.timeoutAddSeconds
        GI.GLib.PRIORITY_DEFAULT
        1 $ do
          resetBottomControlsAlteringVisibilityState
            styleContext
            alteringBottomControlsBoxVisibilityRef
          return False
      return $ Just idd

fadeOutBottomControlsBox :: R.Application -> IO (Maybe Word32)
fadeOutBottomControlsBox
  R.Application
    { R.guiObjects =
        R.GuiObjects
          { R.bottomControlsBox = bottomControlsBox
          }
    , R.ioRefs =
        R.IORefs
          { R.alteringBottomControlsBoxVisibilityRef =
              alteringBottomControlsBoxVisibilityRef
          }
    }
  = do
  altering <- readIORef alteringBottomControlsBoxVisibilityRef
  hidden   <- not <$> GI.Gtk.widgetGetVisible bottomControlsBox
  if altering || hidden
    then return Nothing
    else do
      atomicWriteIORef alteringBottomControlsBoxVisibilityRef True
      styleContext <- GI.Gtk.widgetGetStyleContext bottomControlsBox
      GI.Gtk.styleContextAddClass styleContext fadeOutClassName
      idd <- GI.GLib.timeoutAddSeconds
        GI.GLib.PRIORITY_DEFAULT
        1 $ do
          GI.Gtk.widgetHide bottomControlsBox
          resetBottomControlsAlteringVisibilityState
            styleContext
            alteringBottomControlsBoxVisibilityRef
          return False
      return $ Just idd

resetBottomControlsAlteringVisibilityState :: GI.Gtk.StyleContext -> IORef Bool -> IO ()
resetBottomControlsAlteringVisibilityState
  styleContext
  ioRef
  = do
  GI.Gtk.styleContextRemoveClass styleContext fadeInClassName
  GI.Gtk.styleContextRemoveClass styleContext fadeOutClassName
  atomicWriteIORef ioRef False

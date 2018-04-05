{-
  Movie Monad
  (C) 2017 David Lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}

module CssStyle where

import Prelude
import Control.Monad
import Data.Word
import Data.Text
import qualified Data.ByteString
import qualified GI.Gdk
import qualified GI.Gtk

import qualified Records as R

import Paths_movie_monad

cssPriority :: Word32
cssPriority =
  fromIntegral GI.Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION :: Word32

applyCss :: R.GuiObjects -> IO ()
applyCss
  guiObjects
  = do
  maybeScreen  <- GI.Gdk.screenGetDefault
  provider     <- GI.Gtk.cssProviderNew
  styleFile    <- getDataFileName "data/style.css"
  styleFile318 <- getDataFileName "data/style-3-18.css"
  styleFile320 <- getDataFileName "data/style-3-20.css"
  isGtkVersionGte310  <- isGtkVersionGte 3 10
  isGtkVersionGte318  <- isGtkVersionGte 3 18
  isGtkVersionGte320  <- isGtkVersionGte 3 20
  case ( maybeScreen
       , isGtkVersionGte310
       , isGtkVersionGte318
       , isGtkVersionGte320
       )
    of
    (Just screen, True, False, False) -> do
      GI.Gtk.cssProviderLoadFromPath provider (Data.Text.pack styleFile)
      GI.Gtk.styleContextAddProviderForScreen
        screen
        provider
        cssPriority
      setScaleSizeForLegacyGtk guiObjects
    (Just screen, True, True, False) -> do
      styleFileContents    <- Data.ByteString.readFile styleFile
      styleFileContents318 <- Data.ByteString.readFile styleFile318
      let totalStyleFileContents =
            Data.ByteString.concat
              [styleFileContents, styleFileContents318]
      GI.Gtk.cssProviderLoadFromData provider totalStyleFileContents
      GI.Gtk.styleContextAddProviderForScreen
        screen
        provider
        cssPriority
      setScaleSizeForLegacyGtk guiObjects
    (Just screen, True, True, True) -> do
      styleFileContents    <- Data.ByteString.readFile styleFile
      styleFileContents318 <- Data.ByteString.readFile styleFile318
      styleFileContents320 <- Data.ByteString.readFile styleFile320
      let totalStyleFileContents =
            Data.ByteString.concat
              [styleFileContents, styleFileContents318, styleFileContents320]
      GI.Gtk.cssProviderLoadFromData provider totalStyleFileContents
      GI.Gtk.styleContextAddProviderForScreen
        screen
        provider
        cssPriority
    _ -> return ()

setScaleSizeForLegacyGtk :: R.GuiObjects -> IO ()
setScaleSizeForLegacyGtk
  R.GuiObjects
    { R.window    = window
    , R.seekScale = seekScale
    }
  = do
  GI.Gtk.containerForall
    window
    (\ windowChild -> do
      windowChildName <- GI.Gtk.widgetGetName windowChild
      when (windowChildName == "GtkPopover") $ do
        maybePopover <- GI.Gtk.castTo GI.Gtk.Popover windowChild
        case maybePopover of
          Just popover ->
            GI.Gtk.containerForall
              popover
              (\ popoverChild -> do
                popoverChildName <- GI.Gtk.widgetGetName popoverChild
                when (popoverChildName == "GtkBox") $ do
                  maybeBox <- GI.Gtk.castTo GI.Gtk.Box popoverChild
                  case maybeBox of
                    Just box ->
                      GI.Gtk.containerForall
                        box
                        (\ boxChild -> do
                          boxChildName <- GI.Gtk.widgetGetName boxChild
                          when (boxChildName == "GtkScale") $ do
                            maybeScale <- GI.Gtk.castTo GI.Gtk.Scale boxChild
                            case maybeScale of
                              Just scale -> GI.Gtk.rangeSetMinSliderSize scale 15
                              _          -> return ()
                        )
                    _ -> return ()
              )
          _ -> return ()
    )
  GI.Gtk.rangeSetMinSliderSize seekScale 15

isGtkVersionGte :: Word32 -> Word32 -> IO Bool
isGtkVersionGte major minor = do
  major' <- GI.Gtk.getMajorVersion
  minor' <- GI.Gtk.getMinorVersion
  return (major' >= major && minor' >= minor)

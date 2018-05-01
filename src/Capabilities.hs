{-
  Movie Monad
  (C) 2017 David Lettier
  lettier.com
-}

{-# LANGUAGE
    OverloadedStrings
#-}

module Capabilities where

import Data.Text
import qualified Data.ByteString.Char8
import qualified GI.Gtk

import CssStyle
import Utils
import qualified Records as R

checkCapabilitiesAndNotify :: R.Application -> IO ()
checkCapabilitiesAndNotify
  R.Application
    { R.guiObjects =
        R.GuiObjects
          { R.fileChooserDialogButtonImage = fileChooserDialogButtonImage
          , R.fileChooserDialogLabel       = fileChooserDialogLabel
          }
    }
  = do
    inspectString        <- getGtkInspectOutput
    hasGstDiscoverer'    <- hasGstDiscoverer
    let hasGtkSink'      = hasGtkSink inspectString
    let hasGstLibav'     = hasGstLibav inspectString
    let hasCommonCodecs' = hasCommonCodecs inspectString
    GI.Gtk.widgetShow fileChooserDialogLabel
    case (hasGstDiscoverer', hasGtkSink', hasGstLibav', hasCommonCodecs') of
      (False, _, _, _) -> do
        setImageToIcon "gtk-dialog-error"
        setLabelText "\"gst-discoverer-1.0\" not found. Cannot play videos. Install the GStreamer 1.0 base plugins."
        setLabelStyle "#eb3b5a" "white"
      (_, False, _, _) -> do
        setImageToIcon "gtk-dialog-error"
        setLabelText "\"gtksink\" not found. No videos will show. Install the GStreamer 1.0 bad plugins version 1.8 or higher."
        setLabelStyle "#eb3b5a" "white"
      (_, _, False, _) -> do
        setImageToIcon "gtk-dialog-warning"
        setLabelText "\"gst-libav\" not found. Not every file will play."
        setLabelStyle "#fed330" "#1e272e"
      (_, _, _, False) -> do
        setImageToIcon "gtk-dialog-warning"
        setLabelText "Some common codecs are missing. Not every file will play."
        setLabelStyle "#fed330" "#1e272e"
      (_, _, _, _)     -> do
        GI.Gtk.widgetHide fileChooserDialogLabel
        setImageToIcon "gtk-open"
        setLabelText ""
        setLabelStyle "transparent" "inherit"
  where
    setImageToIcon :: Text -> IO ()
    setImageToIcon iconName =
        GI.Gtk.imageSetFromIconName
          fileChooserDialogButtonImage
          (Just iconName)
          (enumToInt32 GI.Gtk.IconSizeButton)
    setLabelText :: Text -> IO ()
    setLabelText =
      GI.Gtk.labelSetText
        fileChooserDialogLabel
    setLabelStyle :: Text -> Text -> IO ()
    setLabelStyle backgroundColor color = do
      let style =
            Data.ByteString.Char8.pack $
              Data.Text.unpack $
                Data.Text.concat
                  [ "#file-chooser-dialog-label { background-color: "
                  , backgroundColor
                  , "; "
                  , "color: "
                  , color
                  , "; font-size: 13px; padding: 2px; }"
                  ]
      styleWidget
        fileChooserDialogLabel
        style

hasGstDiscoverer :: IO Bool
hasGstDiscoverer = do
  (_, out, _) <- safeRunProcessGetOutput "gst-discoverer-1.0" []
  return $
    Data.Text.isInfixOf "gst-discoverer-1.0" $
      Data.Text.toLower $
        Data.Text.pack out

hasGtkSink :: String -> Bool
hasGtkSink = hasGstInspectText "gtksink"

hasGstLibav :: String -> Bool
hasGstLibav = hasGstInspectText "libav"

hasCommonCodecs :: String -> Bool
hasCommonCodecs haystack =
  hasGstInspectText "avdec_h264" haystack &&
    hasGstInspectText "avdec_aac" haystack

hasGstInspectText :: Text -> String -> Bool
hasGstInspectText needle haystack =
  Data.Text.isInfixOf needle $
    Data.Text.toLower $
      Data.Text.pack haystack

getGtkInspectOutput :: IO String
getGtkInspectOutput = do
  (_, out, _) <- safeRunProcessGetOutput "gst-inspect-1.0" []
  return out

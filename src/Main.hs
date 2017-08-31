{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude
import Foreign.C.Types
import System.Process
import System.Exit
import Control.Monad
import Control.Exception
import Data.Maybe
import Data.Int
import Data.Text
import Data.String.Utils
import Data.GI.Base
import Data.GI.Base.Properties
import GI.GLib
import GI.GObject
import qualified GI.Gtk
import GI.Gst
import GI.GstVideo
import GI.GdkX11
import Paths_movie_monad

-- Declare Element a type instance of IsVideoOverlay via a newtype wrapper
-- Our GStreamer element is playbin
-- Playbin implements the GStreamer VideoOverlay interface
newtype GstElement = GstElement GI.Gst.Element
instance GI.GstVideo.IsVideoOverlay GstElement

main :: IO ()
main = do
  _ <- GI.Gst.init Nothing
  _ <- GI.Gtk.init Nothing

  gladeFile <- getDataFileName "data/gui.glade"
  builder <- GI.Gtk.builderNewFromFile (pack gladeFile)

  window <- builderGetObject GI.Gtk.Window builder "window"
  fileChooserButton <- builderGetObject GI.Gtk.FileChooserButton builder "file-chooser-button"
  drawingArea <- builderGetObject GI.Gtk.Widget builder "drawing-area"
  seekScale <- builderGetObject GI.Gtk.Scale builder "seek-scale"
  onOffSwitch <- builderGetObject GI.Gtk.Switch builder "on-off-switch"
  volumeButton <- builderGetObject GI.Gtk.VolumeButton builder "volume-button"
  desiredVideoWidthComboBox <- builderGetObject GI.Gtk.ComboBoxText builder "desired-video-width-combo-box"
  errorMessageDialog <- builderGetObject GI.Gtk.MessageDialog builder "error-message-dialog"
  aboutButton <- builderGetObject GI.Gtk.Button builder "about-button"
  aboutDialog <- builderGetObject GI.Gtk.AboutDialog builder "about-dialog"

  playbin <- fromJust <$> GI.Gst.elementFactoryMake "playbin" (Just "MultimediaPlayer")

  _ <- GI.Gtk.onWidgetRealize drawingArea $ do
    gdkWindow <- fromJust <$> GI.Gtk.widgetGetWindow drawingArea
    x11Window <- GI.Gtk.unsafeCastTo GI.GdkX11.X11Window gdkWindow

    xid <- GI.GdkX11.x11WindowGetXid x11Window
    let xid' = fromIntegral xid :: CUIntPtr

    GI.GstVideo.videoOverlaySetWindowHandle (GstElement playbin) xid'

  _ <- GI.Gtk.onFileChooserButtonFileSet fileChooserButton $ do
    _ <- GI.Gst.elementSetState playbin GI.Gst.StateNull

    filename <- fromJust <$> GI.Gtk.fileChooserGetFilename fileChooserButton
    let uri = "file://" ++ filename

    volume <- GI.Gtk.scaleButtonGetValue volumeButton
    Data.GI.Base.Properties.setObjectPropertyDouble playbin "volume" volume
    Data.GI.Base.Properties.setObjectPropertyString playbin "uri" (Just $ pack uri)

    desiredVideoWidth <- getDesiredVideoWidth desiredVideoWidthComboBox
    (success, width, height) <- getWindowSize desiredVideoWidth filename

    if success
      then do
        _ <- GI.Gst.elementSetState playbin GI.Gst.StatePlaying
        GI.Gtk.switchSetActive onOffSwitch True
        setWindowSize width height fileChooserButton drawingArea window
      else do
        _ <- GI.Gst.elementSetState playbin GI.Gst.StatePaused
        GI.Gtk.switchSetActive onOffSwitch False
        resetWindowSize desiredVideoWidth fileChooserButton drawingArea window
        _ <- GI.Gtk.onDialogResponse errorMessageDialog (\ _ -> GI.Gtk.widgetHide errorMessageDialog)
        void $ GI.Gtk.dialogRun errorMessageDialog

  _ <- GI.Gtk.onSwitchStateSet onOffSwitch $ \ switchOn -> do
    if switchOn
      then void $ GI.Gst.elementSetState playbin GI.Gst.StatePlaying
      else void $ GI.Gst.elementSetState playbin GI.Gst.StatePaused
    return switchOn

  _ <- GI.Gtk.onScaleButtonValueChanged volumeButton $
      \ volume -> void $ Data.GI.Base.Properties.setObjectPropertyDouble playbin "volume" volume

  seekScaleHandlerId <- GI.Gtk.onRangeValueChanged seekScale $ do
    (couldQueryDuration, duration) <- GI.Gst.elementQueryDuration playbin GI.Gst.FormatTime

    when couldQueryDuration $ do
      percentage' <- GI.Gtk.rangeGetValue seekScale
      let percentage = percentage' / 100.0
      let position = fromIntegral (round ((fromIntegral duration :: Double) * percentage) :: Int) :: Int64
      void $ GI.Gst.elementSeekSimple playbin GI.Gst.FormatTime [ GI.Gst.SeekFlagsFlush ] position

  _ <- GI.GLib.timeoutAddSeconds GI.GLib.PRIORITY_DEFAULT 1 $ do
    (couldQueryDuration, duration) <- GI.Gst.elementQueryDuration playbin GI.Gst.FormatTime
    (couldQueryPosition, position) <- GI.Gst.elementQueryPosition playbin GI.Gst.FormatTime

    let percentage =
          if couldQueryDuration && couldQueryPosition && duration > 0
            then 100.0 * (fromIntegral position / fromIntegral duration :: Double)
            else 0.0

    GI.GObject.signalHandlerBlock seekScale seekScaleHandlerId
    GI.Gtk.rangeSetValue seekScale percentage
    GI.GObject.signalHandlerUnblock seekScale seekScaleHandlerId

    return True

  _ <- GI.Gtk.onComboBoxChanged desiredVideoWidthComboBox $ do
    filename' <- GI.Gtk.fileChooserGetFilename fileChooserButton
    let filename = fromMaybe "" filename'

    desiredVideoWidth <- getDesiredVideoWidth desiredVideoWidthComboBox
    (success, width, height) <- getWindowSize desiredVideoWidth filename

    if success
      then setWindowSize width height fileChooserButton drawingArea window
      else resetWindowSize desiredVideoWidth fileChooserButton drawingArea window

  _ <- GI.Gtk.onWidgetButtonReleaseEvent aboutButton $ \ _ -> do
    _ <- GI.Gtk.onDialogResponse aboutDialog (\ _ -> GI.Gtk.widgetHide aboutDialog)
    void $ GI.Gtk.dialogRun aboutDialog
    return True

  _ <- GI.Gtk.onWidgetDestroy window $ do
    _ <- GI.Gst.elementSetState playbin GI.Gst.StateNull
    _ <- GI.Gst.objectUnref playbin
    GI.Gtk.mainQuit

  GI.Gtk.widgetShowAll window
  GI.Gtk.main

builderGetObject ::
  (GI.GObject.GObject b, GI.Gtk.IsBuilder a) =>
  (Data.GI.Base.ManagedPtr b -> b) ->
  a ->
  Prelude.String ->
  IO b
builderGetObject objectTypeClass builder objectId =
  fromJust <$> GI.Gtk.builderGetObject builder (pack objectId) >>=
    GI.Gtk.unsafeCastTo objectTypeClass

getVideoInfo :: Prelude.String -> Prelude.String -> IO (Bool, Prelude.String)
getVideoInfo flag filename = do
  (code, out, _) <- catch (
      readProcessWithExitCode
        "exiftool"
        [flag, "-s", "-S", filename]
        ""
    ) (\ (_ :: Control.Exception.IOException) -> return (ExitFailure 1, "", ""))
  return (code == System.Exit.ExitSuccess, out)

isVideo :: Prelude.String -> IO Bool
isVideo filename = do
  (success, out) <- getVideoInfo "-MIMEType" filename
  return (success && isInfixOf "video" (pack out))

getWindowSize :: Int -> Prelude.String -> IO (Bool, Int32, Int32)
getWindowSize desiredVideoWidth filename = do
  let defaultWidth = 800
  let defaultHeight = 600

  video <- isVideo filename

  if video
    then do
      (success, out) <- getVideoInfo "-ImageSize" filename
      if success && isInfixOf "x" (pack out)
        then do
          let (width''':height''':_) =
                Data.String.Utils.split "x" $ Data.String.Utils.strip out

          let width'' = read width''' :: Int
          let height'' = read height''' :: Int

          let ratio = fromIntegral height'' / fromIntegral width'' :: Double
          let width' = fromIntegral desiredVideoWidth :: Double
          let height' = width' * ratio
          let width = fromIntegral (round width' :: Int) :: Int32
          let height = fromIntegral (round height' :: Int) :: Int32

          return (True, width, height)
        else return (False, defaultHeight, defaultWidth)
    else return (False, defaultHeight, defaultWidth)

getDesiredVideoWidth :: GI.Gtk.ComboBoxText -> IO Int
getDesiredVideoWidth = fmap (\ x -> read (Data.Text.unpack x) :: Int) . GI.Gtk.comboBoxTextGetActiveText

setWindowSize ::
  Int32 ->
  Int32 ->
  GI.Gtk.FileChooserButton ->
  GI.Gtk.Widget ->
  GI.Gtk.Window ->
  IO ()
setWindowSize width height fileChooserButton drawingArea window = do
  GI.Gtk.setWidgetWidthRequest fileChooserButton width

  GI.Gtk.setWidgetWidthRequest drawingArea width
  GI.Gtk.setWidgetHeightRequest drawingArea height

  GI.Gtk.setWidgetWidthRequest window width
  GI.Gtk.setWidgetHeightRequest window height
  GI.Gtk.windowResize window width (if height <= 0 then 1 else height)

resetWindowSize ::
  (Integral a) =>
  a ->
  GI.Gtk.FileChooserButton ->
  GI.Gtk.Widget ->
  GI.Gtk.Window ->
  IO ()
resetWindowSize width' fileChooserButton drawingArea window = do
  let width = fromIntegral width' :: Int32
  setWindowSize width 0 fileChooserButton drawingArea window

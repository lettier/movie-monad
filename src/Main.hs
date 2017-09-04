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
import Text.Read
import Data.IORef
import Data.Maybe
import Data.Int
import Data.Text
import Data.GI.Base
import Data.GI.Base.Signals
import Data.GI.Base.Properties
import GI.GLib
import GI.GObject
import qualified GI.Gtk
import GI.Gst
import GI.GstVideo
import GI.Gdk
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
  fullscreenButton <- builderGetObject GI.Gtk.Button builder "fullscreen-button"
  errorMessageDialog <- builderGetObject GI.Gtk.MessageDialog builder "error-message-dialog"
  aboutButton <- builderGetObject GI.Gtk.Button builder "about-button"
  aboutDialog <- builderGetObject GI.Gtk.AboutDialog builder "about-dialog"

  playbin <- fromJust <$> GI.Gst.elementFactoryMake "playbin" (Just "MultimediaPlayer")

  isWindowFullScreenRef <- newIORef False

  _ <- GI.Gtk.onWidgetRealize drawingArea $ onDrawingAreaRealize drawingArea playbin fullscreenButton

  _ <- GI.Gtk.onFileChooserButtonFileSet fileChooserButton $
    onFileChooserButtonFileSet
      playbin
      fileChooserButton
      volumeButton
      isWindowFullScreenRef
      desiredVideoWidthComboBox
      onOffSwitch
      fullscreenButton
      drawingArea
      window
      errorMessageDialog

  _ <- GI.Gtk.onSwitchStateSet onOffSwitch (onSwitchStateSet playbin)

  _ <- GI.Gtk.onScaleButtonValueChanged volumeButton (onScaleButtonValueChanged playbin)

  seekScaleHandlerId <- GI.Gtk.onRangeValueChanged seekScale (onRangeValueChanged playbin seekScale)

  _ <- GI.GLib.timeoutAddSeconds GI.GLib.PRIORITY_DEFAULT 1 (updateSeekScale playbin seekScale seekScaleHandlerId)

  _ <- GI.Gtk.onComboBoxChanged desiredVideoWidthComboBox $
      onComboBoxChanged fileChooserButton desiredVideoWidthComboBox drawingArea window

  _ <- GI.Gtk.onWidgetButtonReleaseEvent fullscreenButton
      (onFullscreenButtonRelease isWindowFullScreenRef desiredVideoWidthComboBox fileChooserButton window)

  _ <- GI.Gtk.onWidgetWindowStateEvent window (onWidgetWindowStateEvent isWindowFullScreenRef)

  _ <- GI.Gtk.onWidgetButtonReleaseEvent aboutButton (onAboutButtonRelease aboutDialog)

  _ <- GI.Gtk.onWidgetDestroy window (onWindowDestroy playbin)

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

onDrawingAreaRealize ::
  GI.Gtk.Widget ->
  GI.Gst.Element ->
  GI.Gtk.Button ->
  GI.Gtk.WidgetRealizeCallback
onDrawingAreaRealize drawingArea playbin fullscreenButton = do
  gdkWindow <- fromJust <$> GI.Gtk.widgetGetWindow drawingArea
  x11Window <- GI.Gtk.unsafeCastTo GI.GdkX11.X11Window gdkWindow

  xid <- GI.GdkX11.x11WindowGetXid x11Window
  let xid' = fromIntegral xid :: CUIntPtr

  GI.GstVideo.videoOverlaySetWindowHandle (GstElement playbin) xid'

  GI.Gtk.widgetHide fullscreenButton

onFileChooserButtonFileSet ::
  GI.Gst.Element ->
  GI.Gtk.FileChooserButton ->
  GI.Gtk.VolumeButton ->
  IORef Bool ->
  GI.Gtk.ComboBoxText ->
  GI.Gtk.Switch ->
  GI.Gtk.Button ->
  GI.Gtk.Widget ->
  GI.Gtk.Window ->
  GI.Gtk.MessageDialog ->
  GI.Gtk.FileChooserButtonFileSetCallback
onFileChooserButtonFileSet
  playbin
  fileChooserButton
  volumeButton
  isWindowFullScreenRef
  desiredVideoWidthComboBox
  onOffSwitch
  fullscreenButton
  drawingArea
  window
  errorMessageDialog
  = do
  _ <- GI.Gst.elementSetState playbin GI.Gst.StateNull

  filename <- fromJust <$> GI.Gtk.fileChooserGetFilename fileChooserButton

  setPlaybinUriAndVolume playbin filename volumeButton

  isWindowFullScreen <- readIORef isWindowFullScreenRef

  desiredVideoWidth <- getDesiredVideoWidth desiredVideoWidthComboBox
  maybeWindowSize <- getWindowSize desiredVideoWidth filename

  case maybeWindowSize of
    Nothing -> do
      _ <- GI.Gst.elementSetState playbin GI.Gst.StatePaused
      GI.Gtk.windowUnfullscreen window
      GI.Gtk.switchSetActive onOffSwitch False
      GI.Gtk.widgetHide fullscreenButton
      GI.Gtk.widgetShow desiredVideoWidthComboBox
      resetWindowSize desiredVideoWidth fileChooserButton drawingArea window
      _ <- GI.Gtk.onDialogResponse errorMessageDialog (\ _ -> GI.Gtk.widgetHide errorMessageDialog)
      void $ GI.Gtk.dialogRun errorMessageDialog
    Just (width, height) -> do
      _ <- GI.Gst.elementSetState playbin GI.Gst.StatePlaying
      GI.Gtk.switchSetActive onOffSwitch True
      GI.Gtk.widgetShow fullscreenButton
      unless isWindowFullScreen $ setWindowSize width height fileChooserButton drawingArea window

onSwitchStateSet ::
  GI.Gst.Element ->
  Bool ->
  IO Bool
onSwitchStateSet playbin switchOn = do
  if switchOn
    then void $ GI.Gst.elementSetState playbin GI.Gst.StatePlaying
    else void $ GI.Gst.elementSetState playbin GI.Gst.StatePaused
  return switchOn

onScaleButtonValueChanged ::
  GI.Gst.Element ->
  Double ->
  IO ()
onScaleButtonValueChanged playbin volume =
    void $ Data.GI.Base.Properties.setObjectPropertyDouble playbin "volume" volume

onRangeValueChanged ::
  GI.Gst.Element ->
  GI.Gtk.Scale ->
  IO ()
onRangeValueChanged playbin seekScale = do
  (couldQueryDuration, duration) <- GI.Gst.elementQueryDuration playbin GI.Gst.FormatTime

  when couldQueryDuration $ do
    percentage' <- GI.Gtk.rangeGetValue seekScale
    let percentage = percentage' / 100.0
    let position = fromIntegral (round ((fromIntegral duration :: Double) * percentage) :: Int) :: Int64
    void $ GI.Gst.elementSeekSimple playbin GI.Gst.FormatTime [ GI.Gst.SeekFlagsFlush ] position

updateSeekScale ::
  GI.Gst.Element ->
  GI.Gtk.Scale ->
  Data.GI.Base.Signals.SignalHandlerId ->
  IO Bool
updateSeekScale playbin seekScale seekScaleHandlerId = do
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

onComboBoxChanged ::
  GI.Gtk.FileChooserButton ->
  GI.Gtk.ComboBoxText ->
  GI.Gtk.Widget ->
  GI.Gtk.Window ->
  IO ()
onComboBoxChanged
  fileChooserButton
  desiredVideoWidthComboBox
  drawingArea
  window
  = do
  filename' <- GI.Gtk.fileChooserGetFilename fileChooserButton
  let filename = fromMaybe "" filename'

  desiredVideoWidth <- getDesiredVideoWidth desiredVideoWidthComboBox
  maybeWindowSize <- getWindowSize desiredVideoWidth filename

  case maybeWindowSize of
    Nothing -> resetWindowSize desiredVideoWidth fileChooserButton drawingArea window
    Just (width, height) -> setWindowSize width height fileChooserButton drawingArea window

onFullscreenButtonRelease ::
  IORef Bool ->
  GI.Gtk.ComboBoxText ->
  GI.Gtk.FileChooserButton ->
  GI.Gtk.Window ->
  GI.Gdk.EventButton ->
  IO Bool
onFullscreenButtonRelease
  isWindowFullScreenRef
  desiredVideoWidthComboBox
  fileChooserButton
  window
  _
  = do
  isWindowFullScreen <- readIORef isWindowFullScreenRef
  if isWindowFullScreen
    then do
      GI.Gtk.widgetShow desiredVideoWidthComboBox
      GI.Gtk.widgetShow fileChooserButton
      void $ GI.Gtk.windowUnfullscreen window
    else do
      GI.Gtk.widgetHide desiredVideoWidthComboBox
      GI.Gtk.widgetHide fileChooserButton
      void $ GI.Gtk.windowFullscreen window
  return True

onWidgetWindowStateEvent ::
  IORef Bool ->
  GI.Gdk.EventWindowState ->
  IO Bool
onWidgetWindowStateEvent isWindowFullScreenRef eventWindowState = do
  windowStates <- GI.Gdk.getEventWindowStateNewWindowState eventWindowState
  let isWindowFullScreen = Prelude.foldl (\ acc x -> acc || GI.Gdk.WindowStateFullscreen == x) False windowStates
  writeIORef isWindowFullScreenRef isWindowFullScreen
  return True

onAboutButtonRelease ::
  GI.Gtk.AboutDialog ->
  GI.Gdk.EventButton ->
  IO Bool
onAboutButtonRelease aboutDialog _ = do
  _ <- GI.Gtk.onDialogResponse aboutDialog (\ _ -> GI.Gtk.widgetHide aboutDialog)
  _ <- GI.Gtk.dialogRun aboutDialog
  return True

onWindowDestroy ::
  GI.Gst.Element ->
  IO ()
onWindowDestroy playbin = do
  _ <- GI.Gst.elementSetState playbin GI.Gst.StateNull
  _ <- GI.Gst.objectUnref playbin
  GI.Gtk.mainQuit

setPlaybinUriAndVolume ::
  GI.Gst.Element ->
  Prelude.String ->
  GI.Gtk.VolumeButton ->
  IO ()
setPlaybinUriAndVolume playbin filename volumeButton = do
  let uri = "file://" ++ filename
  volume <- GI.Gtk.scaleButtonGetValue volumeButton
  Data.GI.Base.Properties.setObjectPropertyDouble playbin "volume" volume
  Data.GI.Base.Properties.setObjectPropertyString playbin "uri" (Just $ pack uri)

getVideoInfo :: Prelude.String -> Prelude.String -> IO (Maybe Prelude.String)
getVideoInfo flag filename = do
  (code, out, _) <- catch (
      readProcessWithExitCode
        "exiftool"
        [flag, "-s", "-S", filename]
        ""
    ) (\ (_ :: Control.Exception.IOException) -> return (ExitFailure 1, "", ""))
  if code == System.Exit.ExitSuccess
    then return (Just out)
    else return Nothing

isVideo :: Prelude.String -> IO Bool
isVideo filename = do
  maybeOut <- getVideoInfo "-MIMEType" filename
  case maybeOut of
    Nothing -> return False
    Just out -> return ("video" `isInfixOf` pack out)

getWindowSize :: Int -> Prelude.String -> IO (Maybe (Int32, Int32))
getWindowSize desiredVideoWidth filename =
  isVideo filename >>=
  getWidthHeightString >>=
  splitWidthHeightString >>=
  widthHeightToDouble >>=
  ratio >>=
  windowSize
  where
    getWidthHeightString :: Bool -> IO (Maybe Prelude.String)
    getWidthHeightString False = return Nothing
    getWidthHeightString True = getVideoInfo "-ImageSize" filename
    splitWidthHeightString :: Maybe Prelude.String -> IO (Maybe [Text])
    splitWidthHeightString Nothing = return Nothing
    splitWidthHeightString (Just string) = return (Just (Data.Text.splitOn "x" (pack string)))
    widthHeightToDouble :: Maybe [Text] -> IO (Maybe Double, Maybe Double)
    widthHeightToDouble (Just (x:y:_)) = return (readMaybe (unpack x) :: Maybe Double, readMaybe (unpack y) :: Maybe Double)
    widthHeightToDouble _ = return (Nothing, Nothing)
    ratio :: (Maybe Double, Maybe Double) -> IO (Maybe Double)
    ratio (Just width, Just height) =
      if width <= 0.0 then return Nothing else return (Just (height / width))
    ratio _ = return Nothing
    windowSize :: Maybe Double -> IO (Maybe (Int32, Int32))
    windowSize Nothing = return Nothing
    windowSize (Just ratio') =
      return (Just (fromIntegral desiredVideoWidth :: Int32, round ((fromIntegral desiredVideoWidth :: Double) *  ratio') :: Int32))

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
  GI.Gtk.widgetQueueDraw drawingArea
  setWindowSize width 0 fileChooserButton drawingArea window

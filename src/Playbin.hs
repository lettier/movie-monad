{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}

module Playbin where

import Control.Monad
import Foreign.C
import Foreign.Ptr
import Data.Bits
import Data.Text
import Data.Maybe
import Data.GI.Base.Properties
import Data.GI.Base.ManagedPtr
import qualified GI.GLib
import qualified GI.Gtk
import qualified GI.Gst

import qualified Records as R
import Window
import PlayPause
import ErrorMessage
import Uri

foreign import ccall "gst-ffi.h get_text_tag_list"
    c_getTextTagList :: Ptr a -> CInt -> IO (Ptr b)

addPlaybinHandlers :: R.Application -> IO ()
addPlaybinHandlers
  R.Application {
        R.guiObjects = guiObjects@R.GuiObjects {
              R.volumeButton = volumeButton
          }
      , R.playbin = playbin
      , R.playbinBus = playbinBus
    }
  =
  void(
      GI.Gst.busAddWatch playbinBus GI.GLib.PRIORITY_DEFAULT (
          pipelineBusMessageHandler
            guiObjects
            playbin
        )
    ) >>
  void (
      GI.Gtk.onScaleButtonValueChanged volumeButton (volumeButtonValueChangedHandler playbin)
    )

pipelineBusMessageHandler ::
  R.GuiObjects ->
  GI.Gst.Element ->
  GI.Gst.Bus ->
  GI.Gst.Message ->
  IO Bool
pipelineBusMessageHandler
  guiObjects@R.GuiObjects {
        R.seekScale = seekScale
      , R.fileChooserEntry = fileChooserEntry
      , R.fileChooserButtonLabel = fileChooserButtonLabel
      , R.volumeButton = volumeButton
      , R.errorMessageDialog = errorMessageDialog
      , R.bufferingSpinner = bufferingSpinner
      , R.playPauseButton = playPauseButton
      , R.subtitleSelectionComboBox = subtitleSelectionComboBox
    }
  playbin
  _
  message
  = do
  messageTypes <- GI.Gst.getMessageType message
  let messageType = case messageTypes of
                      [] -> GI.Gst.MessageTypeUnknown
                      (msg:_) -> msg
  entryText <- GI.Gtk.entryGetText fileChooserEntry
  labelText <- GI.Gtk.labelGetText fileChooserButtonLabel
  when (
      messageType == GI.Gst.MessageTypeError && (
        (not . Data.Text.null) entryText ||
        labelText /= "Open"
      )
    ) $ do
      (gError, text) <- GI.Gst.messageParseError message
      gErrorText <- GI.Gst.gerrorMessage gError
      putStr ((Data.Text.unpack . Data.Text.unlines) [text, gErrorText])
      GI.Gtk.entrySetText fileChooserEntry ""
      GI.Gtk.labelSetText fileChooserButtonLabel "Open"
      _ <- GI.Gst.elementSetState playbin GI.Gst.StateNull
      setPlaybinUriAndVolume playbin "" volumeButton
      resetWindow guiObjects
      runErrorMessageDialog
        errorMessageDialog
        (Data.Text.concat ["There was a problem trying to play the video \"", entryText, "\"."])
  when (messageType == GI.Gst.MessageTypeBuffering) $ do
    percent <- GI.Gst.messageParseBuffering message
    isPlaying <- isPlayPauseButtonPlaying playPauseButton
    if percent >= 100
      then do
        GI.Gtk.widgetHide bufferingSpinner
        GI.Gtk.setSpinnerActive bufferingSpinner False
        GI.Gtk.widgetSetSensitive seekScale True
        when isPlaying $ void $ GI.Gst.elementSetState playbin GI.Gst.StatePlaying
      else do
        GI.Gtk.widgetShow bufferingSpinner
        GI.Gtk.setSpinnerActive bufferingSpinner True
        GI.Gtk.widgetSetSensitive seekScale False
        void $ GI.Gst.elementSetState playbin GI.Gst.StatePaused
    return ()
  when (messageType == GI.Gst.MessageTypeStreamStart) $ do
    turnOffSubtitles playbin
    nText <- getTextStreamCount playbin
    GI.Gtk.comboBoxTextRemoveAll subtitleSelectionComboBox
    GI.Gtk.comboBoxTextAppend
      subtitleSelectionComboBox
      (Just "-1")
      "None"
    _ <- GI.Gtk.comboBoxSetActiveId subtitleSelectionComboBox (Just "-1")
    GI.Gtk.widgetHide subtitleSelectionComboBox
    when (nText > 0) $
      mapM_ (\ i -> do
        (_, maybeCode) <- getTextTagLanguageNameAndCode playbin i
        case maybeCode of
          Nothing -> return ()
          Just code -> do
            GI.Gtk.widgetShow subtitleSelectionComboBox
            GI.Gtk.comboBoxTextAppend
              subtitleSelectionComboBox
              (Just (Data.Text.pack (show i)))
              code
        ) [0..(nText-1)]
  return True

volumeButtonValueChangedHandler ::
  GI.Gst.Element ->
  Double ->
  IO ()
volumeButtonValueChangedHandler playbin volume =
    void $ Data.GI.Base.Properties.setObjectPropertyDouble playbin "volume" volume

setPlaybinUriAndVolume :: GI.Gst.Element -> Prelude.String -> GI.Gtk.VolumeButton -> IO ()
setPlaybinUriAndVolume playbin fileName volumeButton = do
  uri <- addUriSchemeIfNone fileName
  volume <- GI.Gtk.scaleButtonGetValue volumeButton
  Data.GI.Base.Properties.setObjectPropertyDouble playbin "volume" volume
  Data.GI.Base.Properties.setObjectPropertyString playbin "uri" (Just $ pack uri)

getTextTagLanguageNameAndCode :: GI.Gst.Element -> Int -> IO (Maybe Text, Maybe Text)
getTextTagLanguageNameAndCode playbin streamId = do
  nText <- getTextStreamCount playbin
  if streamId >= 0 && streamId < nText
    then
      withManagedPtr playbin $ \ playbinPtr -> do
        let streamId' = fromIntegral streamId :: CInt
        tagListPtr <- c_getTextTagList playbinPtr streamId'
        if tagListPtr == nullPtr
          then return (Nothing, Nothing)
          else do
            tagList <- wrapBoxed GI.Gst.TagList tagListPtr
            tagListAsString <- fmap (fromMaybe "") (GI.Gst.tagListToString tagList)
            (successName, name) <- if "language-name" `Data.Text.isInfixOf` tagListAsString
                                    then GI.Gst.tagListGetString tagList "language-name"
                                    else return (False, "")
            (successCode, code) <- if "language-code" `Data.Text.isInfixOf` tagListAsString
                                    then GI.Gst.tagListGetString tagList "language-code"
                                    else return (False, "")
            return (
                  if successName then Just name else Nothing
                , if successCode then Just code else Nothing
              )
    else return (Nothing, Nothing)

turnOnSubtitles :: GI.Gst.Element -> IO ()
turnOnSubtitles playbin = do
  -- Flags "GstPlayFlags" Default: 0x00000617, "soft-colorbalance+deinterlace+soft-volume+text+audio+video"
  --     (0x00000001): video             - Render the video stream
  --     (0x00000002): audio             - Render the audio stream
  --     (0x00000004): text              - Render subtitles
  --     (0x00000008): vis               - Render visualisation when no video is present
  --     (0x00000010): soft-volume       - Use software volume
  --     (0x00000020): native-audio      - Only use native audio formats
  --     (0x00000040): native-video      - Only use native video formats
  --     (0x00000080): download          - Attempt progressive download buffering
  --     (0x00000100): buffering         - Buffer demuxed/parsed data
  --     (0x00000200): deinterlace       - Deinterlace video if necessary
  --     (0x00000400): soft-colorbalance - Use software color balance
  --     (0x00000800): force-filters     - Force audio/video filter(s) to be applied
  -- 0110 0001 0111
  let flags = flip setBit 10 $ flip setBit 9 $ flip setBit 4 $ flip setBit 2 $ flip setBit 1 $ bit 0
  void $ Data.GI.Base.Properties.setObjectPropertyInt playbin "flags" flags

turnOffSubtitles :: GI.Gst.Element -> IO ()
turnOffSubtitles playbin = do
  let flags = flip setBit 10 $ flip setBit 9 $ flip setBit 4 $ flip setBit 1 $ bit 0
  void $ Data.GI.Base.Properties.setObjectPropertyInt playbin "flags" flags

getTextStreamCount :: GI.Gst.Element -> IO Int
getTextStreamCount playbin =
    Data.GI.Base.Properties.getObjectPropertyInt playbin "n-text"
  >>= \ x -> return (if x < 0 then 0 else fromIntegral x :: Int)

getCurrentTextStreamId :: GI.Gst.Element -> IO Int
getCurrentTextStreamId playbin =
  Data.GI.Base.Properties.getObjectPropertyInt playbin "current-text" >>= \ x -> return (fromIntegral x :: Int)

setCurrentTextStreamId :: GI.Gst.Element -> Int -> IO ()
setCurrentTextStreamId playbin streamId =
  Data.GI.Base.Properties.setObjectPropertyInt playbin "current-text" (fromIntegral streamId :: CInt)

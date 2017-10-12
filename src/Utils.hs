{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

module Utils where

import System.FilePath
import Data.Text
import Data.Int
import qualified GI.Gtk

fileNameFromFilePathName :: Data.Text.Text -> Data.Text.Text
fileNameFromFilePathName = Data.Text.pack . System.FilePath.takeFileName . Data.Text.unpack

enumToInt32 :: (Enum a, Ord a) => a -> Int32
enumToInt32 enum = fromIntegral (fromEnum enum) :: Int32

isTextEmpty :: Data.Text.Text -> Bool
isTextEmpty = Data.Text.null . Data.Text.strip

getSelectedVideoWidth :: GI.Gtk.ComboBoxText -> IO Int
getSelectedVideoWidth = fmap (\ x -> read (Data.Text.unpack x) :: Int) . GI.Gtk.comboBoxTextGetActiveText

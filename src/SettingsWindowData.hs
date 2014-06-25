{-# LANGUAGE CPP #-}
module SettingsWindowData where

import Graphics.HsExif
import qualified Data.Map as Map
import FrameRenderer

demoFilename :: FilePath
#ifdef CABAL_OS_WINDOWS
demoFilename = "c:\\Users\\user\\Pictures\\2014\\filename.jpg"
#else
demoFilename = "/home/user/Pictures/2014/filename.jpg"
#endif

fakeImageInfo :: ImageInfo
fakeImageInfo = ImageInfo demoFilename $ Map.fromList [
	(exposureTime, ExifText "1/160"),
	(fnumber, ExifText "3.6"),
	(isoSpeedRatings, ExifText "200"),
	(exposureBiasValue, ExifText "0.00"),
	(make, ExifText "SONY"),
	(model, ExifText "NEX-3N"),
	(software, ExifText "gimp-2.8.2"),
	(copyright, ExifText "copyright 2014"),
	(focalLengthIn35mmFilm, ExifText "32"),
	(dateTimeOriginal, ExifText "2014:06:15 15:52:00")]

contentsComboData :: [(String, String)]
contentsComboData = fmap toPair placeHolders
	where toPair v = (desc v, niceDefault v)

completionComboData :: [(String, String)]
completionComboData = fmap toPair placeHolders
	where toPair v = (desc v, code v)

data PlaceHolder = PlaceHolder
	{
		desc :: String,
		code :: String,
		niceDefault :: String
	}

placeHolders :: [PlaceHolder]
placeHolders = [
	PlaceHolder "File name" "%file" "%file",
	PlaceHolder "Folder name" "%folder" "%folder",
	PlaceHolder "Folder hierarchy" "%folderhier" "%folderhier",
	PlaceHolder "Date" "%date{%x}" "%date{%x}",
	PlaceHolder "Date and time" "%date{%x %R}" "%date{%x %R}",
	PlaceHolder "Date and time, seconds" "%date{%x %X}" "%date{%x %X}",
	PlaceHolder "Exposition time" "%expo" "%expo",
	PlaceHolder "Aperture" "%aper" "f/%aper",
	PlaceHolder "ISO" "%iso" "ISO %iso",
	PlaceHolder "Exposure bias" "%expo_bias" "%expo_bias",
	PlaceHolder "Make" "%make" "%make",
	PlaceHolder "Model" "%model" "%model",
	PlaceHolder "Software" "%soft" "%soft",
	PlaceHolder "Copyright" "%copy" "©%copy",
	PlaceHolder "Focal length (35mm)" "%focal35" "%focal35mm"
	]

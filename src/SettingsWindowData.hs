{-# LANGUAGE CPP #-}
module SettingsWindowData where

import Graphics.HsExif
import qualified Data.Map as Map
import FrameRenderer
import Helpers

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
	PlaceHolder (__ "File name") "%file" "%file",
	PlaceHolder (__ "Folder name") "%folder" "%folder",
	PlaceHolder (__ "Folder hierarchy") "%folderhier" "%folderhier",
	PlaceHolder (__ "Date") "%date{%x}" "%date{%x}",
	PlaceHolder (__ "Date and time") "%date{%x %R}" "%date{%x %R}",
	PlaceHolder (__ "Date and time, seconds") "%date{%x %X}" "%date{%x %X}",
	PlaceHolder (__ "Exposition time") "%expo" "%expo",
	PlaceHolder (__ "Aperture") "%aper" "f/%aper",
	PlaceHolder (__ "ISO") "%iso" "ISO %iso",
	PlaceHolder (__ "Exposure bias") "%expo_bias" "%expo_bias",
	PlaceHolder (__ "Make") "%make" "%make",
	PlaceHolder (__ "Model") "%model" "%model",
	PlaceHolder (__ "Software") "%soft" "%soft",
	PlaceHolder (__ "Copyright") "%copy" "©%copy",
	PlaceHolder (__ "Focal length (35mm)") "%focal35" "%focal35mm"
	]

{-# LANGUAGE TemplateHaskell #-}

module Settings where

import Data.AppSettings as AppSettings
import Control.Lens hiding (Setting, setting)

appName :: String
appName = "imprint"

type ColorRgba = (Double, Double, Double, Double)

data TextStyle = TextStyle
	{
		styleId :: Int,
		textStroke :: ColorRgba,
		textFill :: ColorRgba,
		strokeHeightRatio :: Double,
		fontName :: Maybe String
	} deriving (Show, Read, Eq)
-- http://stackoverflow.com/questions/17132514/
makeLensesWith ?? ''TextStyle $ lensRules
  & lensField .~ (\name -> Just (name ++ "L"))


textStyles :: Setting [TextStyle]
textStyles = ListSetting "textStyles" $ [
	TextStyle {
		styleId = 1,
		textStroke = (1, 0.5, 0, 1),
		textFill = (1, 1, 0, 1),
		strokeHeightRatio = 0.04,
		fontName = Nothing
	},
	TextStyle {
		styleId = 2,
		textStroke = (0, 0, 0, 1),
		textFill = (1, 1, 1, 1),
		strokeHeightRatio = 0.010,
		fontName = Nothing
	},
	TextStyle {
		styleId = 3,
		textStroke = (1, 1, 1, 1),
		textFill = (0, 0, 0, 1),
		strokeHeightRatio = 0.04,
		fontName = Nothing
	}
	]

data ItemPosition = TopLeft
	| TopCenter
	| TopRight
	| BottomLeft
	| BottomCenter
	| BottomRight
	deriving (Show, Read, Eq)

data DisplayItem = DisplayItem
	{
		textStyleId :: Int,
		textSizeFromWidth :: Double,
		marginXFromWidth :: Double,
		marginYFromWidth :: Double,
		position :: ItemPosition
	} deriving (Show, Read, Eq)
--
-- http://stackoverflow.com/questions/17132514/
makeLensesWith ?? ''DisplayItem $ lensRules
  & lensField .~ (\name -> Just (name ++ "L"))

displayItems :: Setting [DisplayItem]
displayItems = ListSetting "displayItems" $ [
	DisplayItem {
		textStyleId = 1,
		textSizeFromWidth = 0.04,
		marginXFromWidth = 0.025,
		marginYFromWidth = 0.025,
		position = BottomRight
	}
	]

getAllSettings :: DefaultConfig
getAllSettings = getDefaultConfig $ do
	setting textStyles
	setting displayItems

readSettings :: IO (Conf, GetSetting)
readSettings = AppSettings.readSettings (AutoFromAppName appName)

saveSettings :: Conf -> IO ()
saveSettings conf = AppSettings.saveSettings getAllSettings (AutoFromAppName appName) conf

-- TODO besides font name also add the date format string

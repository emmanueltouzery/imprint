{-# LANGUAGE TemplateHaskell #-}

module Settings where

import Data.Maybe (fromMaybe)
import System.Directory
import Control.Monad (liftM)
import Data.Map as M
import Control.Monad
import Control.Monad.State
import Data.AppSettings as AppSettings
import Control.Lens hiding (Setting, setting)
import Control.Lens.TH

appName :: String
appName = "picdate"

type ColorRgba = (Double, Double, Double, Double)

data TextStyle = TextStyle
	{
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
		textStroke = (1, 0.5, 0, 1),
		textFill = (1, 1, 0, 1),
		strokeHeightRatio = 0.04,
		fontName = Nothing
	},
	TextStyle {
		textStroke = (0, 0, 0, 1),
		textFill = (1, 1, 1, 1),
		strokeHeightRatio = 0.010,
		fontName = Nothing
	},
	TextStyle {
		textStroke = (1, 1, 1, 1),
		textFill = (0, 0, 0, 1),
		strokeHeightRatio = 0.04,
		fontName = Nothing
	}
	]

selectedTextStyle :: Setting TextStyle
selectedTextStyle = Setting "textStyle" $ TextStyle
	{
		textStroke = (1, 0.5, 0, 1),
		textFill = (1, 1, 0, 1),
		strokeHeightRatio = 0.04,
		fontName = Nothing
	}

textSizeFromWidth :: Setting Double
textSizeFromWidth = Setting "textSizeFromWidth" 0.04

marginXFromWidth :: Setting Double
marginXFromWidth = Setting "marginXFromWidth" 0.025

marginYFromWidth :: Setting Double
marginYFromWidth = Setting "marginYFromWidth" 0.025

getAllSettings :: DefaultConfig
getAllSettings = getDefaultConfig $ do
	setting selectedTextStyle
	setting textStyles
	setting textSizeFromWidth
	setting marginXFromWidth
	setting marginYFromWidth

readSettings :: IO (Conf, GetSetting)
readSettings = AppSettings.readSettings (AutoFromAppName appName)

saveSettings :: Conf -> IO ()
saveSettings conf = AppSettings.saveSettings getAllSettings (AutoFromAppName appName) conf

-- TODO besides font name also add the date format string

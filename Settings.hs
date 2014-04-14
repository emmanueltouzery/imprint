{-# LANGUAGE DeriveGeneric #-}

module Settings where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import GHC.Generics
import System.Directory
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Helpers

data Settings = Settings
	{
		textSizeFromWidth :: Double,
		textStroke :: CairoColor,
		textStrokeWidthFromWidth :: Double,
		textFill :: CairoColor,
		marginXFromWidth :: Double,
		marginYFromWidth :: Double
	} deriving Generic
instance FromJSON Settings
instance ToJSON Settings

defaultSettings = Settings
	{
		textSizeFromWidth = 0.04,
		textStroke = mkCairoColor 1 0.5 0 1,
		textStrokeWidthFromWidth = 0.0016667,
		textFill = mkCairoColor 1 1 0 1,
		marginXFromWidth = 0.025,
		marginYFromWidth = 0.025
	}

getSettingsFolder :: IO FilePath
getSettingsFolder = do
	home <- getHomeDirectory
	let result = home ++ "/.picdate/"
	createDirectoryIfMissing False result
	return result

getConfigFileName :: IO String
getConfigFileName = fmap (++"config.json") getSettingsFolder

readSettings :: IO Settings
readSettings = do
	settingsFile <- getConfigFileName
	isSettings <- doesFileExist settingsFile
	if isSettings
		then liftM decodeOrDefault (BS.readFile settingsFile)
		else return defaultSettings
	where
		decodeOrDefault bs = fromMaybe defaultSettings $ decodeStrict bs

saveSettings :: Settings -> IO ()
saveSettings settings = getConfigFileName >>= flip BL.writeFile json
	where json = encodePretty settings

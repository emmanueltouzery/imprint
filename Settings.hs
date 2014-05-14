module Settings where

import Data.Maybe (fromMaybe)
import System.Directory
import Control.Monad (liftM)
import Data.Map as M
import Control.Monad
import Control.Monad.State
import Data.AppSettings as AppSettings

appName :: String
appName = "picdate"

textSizeFromWidth :: Setting Double
textSizeFromWidth = Setting "textSizeFromWidth" 0.04

textStroke :: Setting (Double,Double,Double,Double)
textStroke = Setting "textStroke" (1, 0.5, 0, 1)

strokeHeightRatio :: Setting Double
strokeHeightRatio = Setting "strokeHeightRatio" 0.05

textFill :: Setting (Double,Double,Double,Double)
textFill = Setting "textFill" (1, 1, 0, 1)

marginXFromWidth :: Setting Double
marginXFromWidth = Setting "marginXFromWidth" 0.025

marginYFromWidth :: Setting Double
marginYFromWidth = Setting "marginYFromWidth" 0.025

getAllSettings :: DefaultConfig
getAllSettings = getDefaultConfig $ do
	setting textSizeFromWidth
	setting textStroke
	setting strokeHeightRatio
	setting textFill
	setting marginXFromWidth
	setting marginYFromWidth

readSettings :: IO (Conf, GetSetting)
readSettings = AppSettings.readSettings (AutoFromAppName appName)

saveSettings :: Conf -> IO ()
saveSettings conf = AppSettings.saveSettings getAllSettings (AutoFromAppName appName) conf

-- TODO besides font name also add the date format string

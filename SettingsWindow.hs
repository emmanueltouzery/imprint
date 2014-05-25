{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module SettingsWindow where

import Graphics.UI.Gtk hiding (styleSet)
import Graphics.Rendering.Cairo hiding (width, height, x)
import Data.IORef
import Data.AppSettings (getSetting', Conf, setSetting)

import TextStylesSettings

showSettingsWindow :: Builder -> IORef Conf -> IO ()
showSettingsWindow builder latestConfig = do
	settingsWindow <- builderGetObject builder castToWindow "settings_window"
	locationPicker <- builderGetObject builder castToDrawingArea "locationpicker"
	widgetSetSizeRequest locationPicker 150 500
	locationPicker `on` draw $ drawCurLocationConfig

	pickTextStyleBtn <- builderGetObject builder castToButton "picktextstylebtn"
	pickTextStyleBtn `on` buttonActivated $ do
		showTextStyleListDialog builder latestConfig settingsWindow

	windowSetDefaultSize settingsWindow 600 500
	--prepareTextStylePreview builder latestConfig
	widgetShowAll settingsWindow

drawCurLocationConfig :: Render ()
drawCurLocationConfig = return ()


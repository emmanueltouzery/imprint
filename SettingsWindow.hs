module SettingsWindow where

import Graphics.UI.Gtk hiding (styleSet)
import Data.IORef
import Data.AppSettings (getSetting', Conf, setSetting)

showSettingsWindow :: Builder -> IORef Conf -> IO ()
showSettingsWindow builder latestConfig = do
	settingsWindow <- builderGetObject builder castToWindow "settings_window"
	locationPicker <- builderGetObject builder castToDrawingArea "locationpicker"
	widgetSetSizeRequest locationPicker 150 500
	windowSetDefaultSize settingsWindow 600 500
	widgetShowAll settingsWindow

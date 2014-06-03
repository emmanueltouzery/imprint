{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module SettingsWindow where

import Graphics.UI.Gtk hiding (styleSet)
import Graphics.Rendering.Cairo hiding (width, height, x)
import Data.IORef
import Data.AppSettings (getSetting', Conf, setSetting)
import Data.List
import Data.Maybe (fromJust)

import TextStylesSettings
import Settings

showSettingsWindow :: Builder -> IORef Conf -> IO ()
showSettingsWindow builder latestConfig = do
	ctxt <- cairoCreateContext Nothing -- TODO creating cairo ctxt all over the place...

	settingsWindow <- builderGetObject builder castToWindow "settings_window"
	locationPicker <- builderGetObject builder castToDrawingArea "locationpicker"
	widgetSetSizeRequest locationPicker 150 500
	locationPicker `on` draw $ drawCurLocationConfig

	pickTextStyleBtn <- builderGetObject builder castToButton "picktextstylebtn"
	pickTextStyleBtn `on` buttonActivated $ do
		showTextStyleListDialog builder latestConfig settingsWindow

	text <- layoutEmpty ctxt
	text `layoutSetText` "2014-04-01"
	textStylePreview <- builderGetObject builder castToDrawingArea "textstylepreview"
	textStylePreview `on` draw $ do
		conf <- liftIO $ readIORef latestConfig
		let cTextStyle = fromJust $ find ((== getSetting' conf selectedTextStyleId) . styleId)
			$ getSetting' conf textStyles
		liftIO $ do
			updateFontFromTextStyle ctxt cTextStyle
			setFontSizeForWidget ctxt text textStylePreview
		renderText text cTextStyle

	windowSetDefaultSize settingsWindow 600 500
	--prepareTextStylePreview builder latestConfig
	widgetShowAll settingsWindow

drawCurLocationConfig :: Render ()
drawCurLocationConfig = return ()


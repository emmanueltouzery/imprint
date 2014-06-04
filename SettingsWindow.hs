{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module SettingsWindow where

import Graphics.UI.Gtk hiding (styleSet)
import Graphics.Rendering.Cairo hiding (width, height, x)
import Data.IORef
import Data.AppSettings (getSetting', Conf, setSetting)
import Data.List
import Data.Maybe (fromJust)
import Control.Monad (liftM)

import TextStylesSettings
import Settings

showSettingsWindow :: Builder -> IORef Conf -> IO ()
showSettingsWindow builder latestConfig = do
	ctxt <- cairoCreateContext Nothing -- TODO creating cairo ctxt all over the place...

	settingsWindow <- builderGetObject builder castToWindow "settings_window"
	imageLayout <- builderGetObject builder castToDrawingArea "image_layout"
	
	aspectRatioCombo <- builderGetObject builder castToComboBox "aspect_ratio_combo"
	aspectRatioCombo `on` changed $ widgetQueueDraw imageLayout

	textLayout <- layoutEmpty ctxt
	textLayout `layoutSetText` "2014-04-01"
	imageLayout `on` draw $ drawImageLayout imageLayout aspectRatioCombo latestConfig ctxt textLayout

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

data AspectRatio = FourThree
	| ThreeTwo

getHeightMultiplier :: ComboBox -> IO Double
getHeightMultiplier aspectRatioCombo = do
	v <- comboBoxGetActive aspectRatioCombo
	return $ case v of
		0 -> 3/4
		_ -> 2/3

drawImageLayout :: DrawingArea -> ComboBox -> IORef Conf -> PangoContext -> PangoLayout -> Render ()
drawImageLayout drawingArea aspectRatioCombo latestConfig ctxt text = do
	heightMultiplier <- liftIO $ getHeightMultiplier aspectRatioCombo
	conf <- liftIO $ readIORef latestConfig

	-- draw image borders...
	w <- liftIO $ liftM fromIntegral $ widgetGetAllocatedWidth drawingArea
	h <- liftIO $ liftM fromIntegral $ widgetGetAllocatedHeight drawingArea

	let (effectiveW, effectiveH) = if w*heightMultiplier < h
		then (w, w*heightMultiplier)
		else (h/heightMultiplier, h)

	rectangle 0 ((h-effectiveH)/2) effectiveW effectiveH
	moveTo 0 0


	-- bottom right corner
	let cTextStyle = fromJust $ find ((== getSetting' conf selectedTextStyleId) . styleId)
		$ getSetting' conf textStyles
	liftIO $ do
		updateFontFromTextStyle ctxt cTextStyle
		setFontSizeForWidget ctxt text drawingArea
	renderText text cTextStyle
	return ()


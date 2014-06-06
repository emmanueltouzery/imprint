{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module SettingsWindow where

import Graphics.UI.Gtk hiding (styleSet)
import Graphics.Rendering.Cairo hiding (width, height, x)
import Data.IORef
import Data.AppSettings (getSetting', Conf, GetSetting(..))
import Data.List
import Data.Maybe (fromJust)
import Control.Monad (liftM)

import TextStylesSettings
import Settings
import FrameRenderer (renderFrame)

showSettingsWindow :: Builder -> IORef Conf -> IO ()
showSettingsWindow builder latestConfig = do
	ctxt <- cairoCreateContext Nothing -- TODO creating cairo ctxt all over the place...

	settingsWindow <- builderGetObject builder castToWindow "settings_window"
	imageLayout <- builderGetObject builder castToDrawingArea "image_layout"
	
	aspectRatioCombo <- builderGetObject builder castToComboBox "aspect_ratio_combo"
	aspectRatioCombo `on` changed $ widgetQueueDraw imageLayout

	textLayout <- layoutEmpty ctxt
	textLayout `layoutSetText` "2014-04-01"
	imageLayout `on` draw $ drawImageLayout imageLayout aspectRatioCombo latestConfig textLayout

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

drawImageLayout :: DrawingArea -> ComboBox -> IORef Conf -> PangoLayout -> Render ()
drawImageLayout drawingArea aspectRatioCombo latestConfig text = do
	heightMultiplier <- liftIO $ getHeightMultiplier aspectRatioCombo
	conf <- liftIO $ readIORef latestConfig

	-- draw image borders...
	w <- liftIO $ liftM fromIntegral $ widgetGetAllocatedWidth drawingArea
	h <- liftIO $ liftM fromIntegral $ widgetGetAllocatedHeight drawingArea

	let (effectiveW, effectiveH) = if w*heightMultiplier < h
		then (w, w*heightMultiplier)
		else (h/heightMultiplier, h)

	let top = ((h-effectiveH)/2)

	rectangle 0 top effectiveW effectiveH
	setSourceRGB 0 0 0
	strokePreserve
	setSourceRGB 1 1 1
	fill
	save
	translate 0 top

	renderFrame (floor effectiveW) (floor effectiveH) text (GetSetting $ getSetting' conf)
	restore
	return ()


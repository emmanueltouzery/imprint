{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module SettingsWindow where

import Graphics.UI.Gtk hiding (styleSet)
import Graphics.Rendering.Cairo hiding (width, height, x)
import Data.IORef
import Data.AppSettings (getSetting', setSetting, Conf)
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import Control.Lens

import TextStylesSettings
import Settings
import FrameRenderer
import GtkViewModel

-- TODO make it a dialog?

showSettingsWindow :: Builder -> IORef Conf -> IO ()
showSettingsWindow builder latestConfig = do
	ctxt <- cairoCreateContext Nothing -- TODO creating cairo ctxt all over the place...

	-- TODO the next two lines are probably useless, since i have
	-- the displayItemModel and i can read its position?
	let startItemPosition = BottomRight
	curItemPosition <- newIORef startItemPosition

	startConf <- readIORef latestConfig
	let startDisplayItem = fromJust $ getDisplayItem startConf startItemPosition
	displayItemModel <- makeModel startDisplayItem

	settingsWindow <- builderGetObject builder castToWindow "settings_window"
	imageLayout <- builderGetObject builder castToDrawingArea "image_layout"
	
	aspectRatioCombo <- builderGetObject builder castToComboBox "aspect_ratio_combo"
	aspectRatioCombo `on` changed $ widgetQueueDraw imageLayout

	textLayout <- layoutEmpty ctxt
	textLayout `layoutSetText` "2014-04-01"
	imageLayout `on` draw $ do
		updatedConf <- liftIO $ updateConfFromModel latestConfig displayItemModel
		drawImageLayout imageLayout aspectRatioCombo updatedConf textLayout ctxt

	pickTextStyleBtn <- builderGetObject builder castToButton "picktextstylebtn"
	pickTextStyleBtn `on` buttonActivated $ do
		conf <- readIORef latestConfig
		itemPosition <- readIORef curItemPosition
		newConf <- showTextStyleListDialog builder conf itemPosition settingsWindow
		writeIORef latestConfig newConf
		-- in case the user changed the text style
		-- for the model, update the model.
		modifyModel displayItemModel (textStyleIdL .~ styleId (getDisplayItemTextStyle newConf itemPosition))
		return ()

	textSizeScale <- builderGetObject builder castToScale "textSizeScale"
	let rangeBindInfo = RangeBindInfo
		{
			range = textSizeScale,
			startV = textSizeFromWidth startDisplayItem *100,
			lowerV = 0,
			upperV = 22,
			stepIncr = 2,
			pageIncr = 1,
			pageSize = 1
		}
	bindModel displayItemModel textSizeFromWidthL rangeBindInfo

	addModelObserver displayItemModel $ \_ -> widgetQueueDraw imageLayout

	text <- layoutEmpty ctxt
	text `layoutSetText` "2014-04-01"
	textStylePreview <- builderGetObject builder castToDrawingArea "textstylepreview"
	textStylePreview `on` draw $ do
		conf <- liftIO $ readIORef latestConfig
		itemPosition <- liftIO $ readIORef curItemPosition
		let cTextStyle = getDisplayItemTextStyle conf itemPosition
		liftIO $ setFontSizeForWidget ctxt text textStylePreview
		renderText text ctxt cTextStyle

	windowSetDefaultSize settingsWindow 600 500
	--prepareTextStylePreview builder latestConfig
	widgetShowAll settingsWindow

updateConfFromModel :: IORef Conf -> Model DisplayItem -> IO Conf
updateConfFromModel latestConfig displayItemModel = do
	conf <- readIORef latestConfig
	curDisplayItem <- liftIO $ readModel displayItemModel
	let curPos = position curDisplayItem
	let updatedDisplayItems = curDisplayItem : filter ((/=curPos) . position) (getSetting' conf displayItems)
	return $ setSetting conf displayItems updatedDisplayItems

data AspectRatio = FourThree
	| ThreeTwo

getHeightMultiplier :: ComboBox -> IO Double
getHeightMultiplier aspectRatioCombo = do
	v <- comboBoxGetActive aspectRatioCombo
	return $ case v of
		0 -> 3/4
		_ -> 2/3

getDisplayItemsStyles :: Conf -> [(DisplayItem, TextStyle)]
getDisplayItemsStyles conf = zip displayItemsV textStylesV
	where
		displayItemsV = getSetting' conf displayItems
		textStylesV = fmap (getDisplayItemTextStyle conf . position) displayItemsV

drawImageLayout :: DrawingArea -> ComboBox -> Conf -> PangoLayout -> PangoContext -> Render ()
drawImageLayout drawingArea aspectRatioCombo conf text ctxt = do
	heightMultiplier <- liftIO $ getHeightMultiplier aspectRatioCombo

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

	renderFrame (floor effectiveW) (floor effectiveH) text ctxt $ getDisplayItemsStyles conf
	restore
	return ()


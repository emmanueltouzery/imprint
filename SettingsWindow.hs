{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module SettingsWindow where

import Graphics.UI.Gtk hiding (styleSet)
import Graphics.Rendering.Cairo hiding (width, height, x)
import Data.IORef
import Data.AppSettings (getSetting', setSetting, Conf)
import Control.Monad (liftM, when)
import Control.Lens
import Data.Maybe (fromJust)
import Data.List

import TextStylesSettings
import Settings
import FrameRenderer
import GtkViewModel
import Helpers

-- TODO make it a dialog?

showSettingsWindow :: Builder -> IORef Conf -> IO ()
showSettingsWindow builder latestConfig = do
	ctxt <- cairoCreateContext Nothing -- TODO creating cairo ctxt all over the place...

	startConf <- readIORef latestConfig
	displayItemsModel <- makeListModel $ getSetting' startConf displayItems
	let getCurItem = liftM fromJust . listModelGetCurrentItem

	-- TODO create bottom-right if there is no display item at all

	settingsWindow <- builderGetObject builder castToWindow "settings_window"
	imageLayout <- builderGetObject builder castToDrawingArea "image_layout"
	
	aspectRatioCombo <- builderGetObject builder castToComboBox "aspect_ratio_combo"
	aspectRatioCombo `on` changed $ widgetQueueDraw imageLayout

	textLayout <- layoutEmpty ctxt
	textLayout `layoutSetText` "2014-04-01"
	imageLayout `on` draw $ do
		updatedConf <- liftIO $ updateConfFromModel latestConfig displayItemsModel
		drawImageLayout imageLayout aspectRatioCombo updatedConf textLayout ctxt

	textStylePreview <- builderGetObject builder castToDrawingArea "textstylepreview"

	pickTextStyleBtn <- builderGetObject builder castToButton "picktextstylebtn"
	pickTextStyleBtn `on` buttonActivated $ do
		conf <- readIORef latestConfig
		displayItemModel <- getCurItem displayItemsModel
		itemPosition <- liftM position $ readModel displayItemModel
		newConf <- showTextStyleListDialog builder conf itemPosition settingsWindow
		writeIORef latestConfig newConf
		-- in case the user changed the text style
		-- for the model, update the model.
		modifyModel displayItemModel (textStyleIdL .~ styleId (getDisplayItemTextStyle newConf itemPosition))
		widgetQueueDraw textStylePreview
		return ()

	textSizeScale <- builderGetObject builder castToScale "textSizeScale"
	let scaleRangeBindInfo = RangeBindInfo
		{
			range = textSizeScale,
			lowerV = 0,
			upperV = 22,
			stepIncr = 2,
			pageIncr = 1,
			pageSize = 1
		}

	horizontalMarginScale <- builderGetObject builder castToScale "horizontalMarginScale"
	let horMarginRangeBindInfo = RangeBindInfo
		{
			range = horizontalMarginScale,
			lowerV = 0,
			upperV = 22,
			stepIncr = 2,
			pageIncr = 1,
			pageSize = 1
		}

	verticalMarginScale <- builderGetObject builder castToScale "verticalMarginScale"
	let verMarginRangeBindInfo = horMarginRangeBindInfo { range = verticalMarginScale }

	text <- layoutEmpty ctxt
	text `layoutSetText` "2014-04-01"
	textStylePreview `on` draw $ do
		conf <- liftIO $ readIORef latestConfig
		curDisplayItem <- liftIO $ getCurItem displayItemsModel >>= readModel
		let cTextStyle = getDisplayItemTextStyle conf $ position curDisplayItem
		liftIO $ setFontSizeForWidget ctxt text textStylePreview
		renderText text ctxt cTextStyle

	addListModelCurrentItemObserver displayItemsModel $ \currentDisplayItemModel -> do
		putStrLn "current display item changed!"
		bindModel currentDisplayItemModel marginXFromWidthL horMarginRangeBindInfo
		bindModel currentDisplayItemModel textSizeFromWidthL scaleRangeBindInfo
		bindModel currentDisplayItemModel marginYFromWidthL verMarginRangeBindInfo
		widgetQueueDraw imageLayout
		widgetQueueDraw textStylePreview

	readListModel displayItemsModel >>= listModelSetCurrentItem displayItemsModel . head

	displayItemPositionCombo <- builderGetObject builder castToComboBox "displayItemPositionCombo"
	liftM position (getCurItem displayItemsModel >>= readModel) >>= comboBoxSelectPosition displayItemPositionCombo
	displayItemPositionCombo `on` changed $
		changeDisplayItemPosition settingsWindow displayItemPositionCombo displayItemsModel

	settingsOkBtn <- builderGetObject builder castToButton "settingsOk"
	settingsOkBtn `on` buttonActivated $ do
		updatedConf <- liftIO $ updateConfFromModel latestConfig displayItemsModel
		saveSettings updatedConf

	windowSetDefaultSize settingsWindow 600 500
	--prepareTextStylePreview builder latestConfig
	widgetShowAll settingsWindow

comboIndexes :: [(Int, ItemPosition)]
comboIndexes = [(0, TopLeft), (1, TopCenter), (2, TopRight),
		(3, BottomLeft), (4, BottomCenter), (5, BottomRight)]

changeDisplayItemPosition :: Window -> ComboBox -> ListModel DisplayItem -> IO ()
changeDisplayItemPosition parent displayItemPositionCombo displayItemsModel = do
	selectedPosition <- comboBoxGetPosition displayItemPositionCombo
	itemsModels <- readListModel displayItemsModel
	items <- mapM readModel itemsModels
	case find ((==selectedPosition) . position . snd) (zip itemsModels items) of
		Nothing -> do
			isCreate <- offerCreate parent selectedPosition displayItemsModel
			when (not isCreate) $ do
				displayItem <- listModelGetCurrentItem displayItemsModel >>= readModel . fromJust
				comboBoxSelectPosition displayItemPositionCombo $ position displayItem
		Just (itemModel, _) -> listModelSetCurrentItem displayItemsModel itemModel

comboBoxGetPosition :: ComboBox -> IO ItemPosition
comboBoxGetPosition combo = do
	comboPosition <- comboBoxGetActive combo 
	case find ((==comboPosition) . fst) comboIndexes of
		Nothing -> error $ "got index " ++ show comboPosition ++ " from the item position combo!?"
		Just (_, pos) -> return pos

comboBoxSelectPosition :: ComboBox -> ItemPosition -> IO ()
comboBoxSelectPosition combo itemPosition = case find ((==itemPosition) . snd) comboIndexes of
	Nothing -> error $ "No combobox entry for position " ++ show itemPosition
	Just (idx, _) -> comboBoxSetActive combo idx

offerCreate :: Window -> ItemPosition -> ListModel DisplayItem -> IO Bool
offerCreate parent itemPosition displayItemsModel = do
	isCreate <- dialogYesNo parent "There is no item to display at that position. Create one?"
	if isCreate
		then do
			displayItemTemplate <- listModelGetCurrentItem displayItemsModel >>= readModel . fromJust
			newDisplayItem <- makeModel $ displayItemTemplate { position = itemPosition }
			listModelAddItem displayItemsModel newDisplayItem
			listModelSetCurrentItem displayItemsModel newDisplayItem
			return True
		else return False

updateConfFromModel :: IORef Conf -> ListModel DisplayItem -> IO Conf
updateConfFromModel latestConfig displayItemsModel = do
	conf <- readIORef latestConfig
	curDisplayItems <- liftIO $ readListModel displayItemsModel >>= mapM readModel
	return $ setSetting conf displayItems curDisplayItems

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


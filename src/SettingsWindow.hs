{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module SettingsWindow where

import Graphics.UI.Gtk hiding (styleSet)
import Graphics.Rendering.Cairo hiding (width, height, x)
import Data.IORef
import Data.AppSettings (getSetting', setSetting, Conf)
import Control.Monad (liftM, when)
import Data.Maybe (fromJust, isJust)
import Data.List
import Graphics.HsExif
import qualified Data.Map as Map

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
	textStylesModel <- makeListModel $ getSetting' startConf textStyles

	let getCurItem = liftM fromJust $ listModelGetCurrentItem displayItemsModel

	-- TODO create bottom-right if there is no display item at all

	settingsWindow <- builderGetObject builder castToWindow "settings_window"
	imageLayout <- builderGetObject builder castToDrawingArea "image_layout"
	
	aspectRatioCombo <- builderGetObject builder castToComboBox "aspect_ratio_combo"
	aspectRatioCombo `on` changed $ widgetQueueDraw imageLayout

	-- TODO turns out too small, workaround, forced height request of 30px
	-- in the gtkbuilder file...
	contentsCombo <- builderGetObject builder castToComboBox "contentscombo"
	comboBoxSetModelText contentsCombo
	let comboContentsBindInfo = ComboBindInfo
		{
			comboWidget = contentsCombo,
			comboValues = contentsComboData,
			defaultIndex = length contentsComboData - 1
		}
	contentsCombo `on` changed $ do
		comboPos <- comboBoxGetActive contentsCombo
		when (comboPos == length contentsComboData-1) $ getCurItem >>= showCustomContentsDialog settingsWindow builder

	textLayout <- layoutEmpty ctxt
	textLayout `layoutSetText` "2014-04-01"
	imageLayout `on` draw $ do
		drawImageLayout imageLayout aspectRatioCombo displayItemsModel textStylesModel textLayout ctxt

	textStylePreview <- builderGetObject builder castToDrawingArea "textstylepreview"

	pickTextStyleBtn <- builderGetObject builder castToButton "picktextstylebtn"
	pickTextStyleBtn `on` buttonActivated $ do
		showTextStyleListDialog builder displayItemsModel textStylesModel settingsWindow
		-- in case the user changed the text style
		-- for the model, redraw.
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
		cTextStyle <- liftIO $ getDisplayItemTextStyle displayItemsModel textStylesModel
		fontSize <- liftIO $ setFontSizeForWidget ctxt text textStylePreview cTextStyle
		renderText text ctxt cTextStyle $ fromIntegral fontSize

	displayItemPositionCombo <- builderGetObject builder castToComboBox "displayItemPositionCombo"

	positionDeleteButton <- builderGetObject builder castToButton "positionDeleteButton"
	positionDeleteButton `on` buttonActivated $ do
		confirm <- dialogYesNo settingsWindow "Are you sure to remove the display item?"
		when confirm $ do
			itemToRemove <- liftM fromJust $ listModelGetCurrentItem displayItemsModel 
			newCurItem <- liftM (find (/= itemToRemove)) $ readListModel displayItemsModel
			case newCurItem of
				Nothing -> displayError settingsWindow "Can't delete the last display item"
				Just newItem -> do
					listModelSetCurrentItem displayItemsModel newItem
					newPos <- liftM position $ readModel newItem
					comboBoxSelectPosition displayItemPositionCombo newPos
					listModelRemoveItem displayItemsModel itemToRemove

	addListModelCurrentItemObserver displayItemsModel $ \currentDisplayItemModel -> do
		putStrLn "current display item changed!"
		bindModel currentDisplayItemModel marginXFromWidthL horMarginRangeBindInfo
		bindModel currentDisplayItemModel textSizeFromWidthL scaleRangeBindInfo
		bindModel currentDisplayItemModel marginYFromWidthL verMarginRangeBindInfo
		bindModel currentDisplayItemModel itemContentsL comboContentsBindInfo
		widgetQueueDraw imageLayout
		widgetQueueDraw textStylePreview
		-- this is so that we redraw when the current item is modified.
		addModelObserver currentDisplayItemModel $ \_ -> do
			widgetQueueDraw imageLayout
			widgetQueueDraw textStylePreview

	readListModel displayItemsModel >>= listModelSetCurrentItem displayItemsModel . head

	liftM position (getCurItem >>= readModel) >>= comboBoxSelectPosition displayItemPositionCombo
	displayItemPositionCombo `on` changed $
		changeDisplayItemPosition settingsWindow displayItemPositionCombo displayItemsModel

	settingsOkBtn <- builderGetObject builder castToButton "settingsOk"
	settingsOkBtn `on` buttonActivated $ do
		updatedConf <- liftIO $ updateConfFromModel latestConfig displayItemsModel textStylesModel
		saveSettings updatedConf

	windowSetDefaultSize settingsWindow 600 500
	--prepareTextStylePreview builder latestConfig
	widgetShowAll settingsWindow

fakeImageInfo :: ImageInfo
fakeImageInfo = ImageInfo "filename.jpg" $ Map.fromList [
	(exposureTime, ExifText "1/160"),
	(fnumber, ExifText "3.6"),
	(isoSpeedRatings, ExifText "200"),
	(exposureBiasValue, ExifText "0.00"),
	(make, ExifText "SONY"),
	(model, ExifText "NEX-3N"),
	(software, ExifText "gimp-2.8.2"),
	(copyright, ExifText "copyright 2014"),
	(focalLengthIn35mmFilm, ExifText "32"),
	(dateTimeOriginal, ExifText "2014:06:15 15:52:00")]

comboIndexes :: [(Int, ItemPosition)]
comboIndexes = [(0, TopLeft), (1, TopCenter), (2, TopRight),
		(3, BottomLeft), (4, BottomCenter), (5, BottomRight)]

contentsComboData :: [(String, String)]
contentsComboData = [
	("Filename", "%file"),
	("Date", "%date{%x}"),
	("Date and time", "%date{%x %R}"),
	("Date and time, seconds", "%date{%x %X}"),
	("Exposition time", "%expo"),
	("Aperture", "f/%aper"),
	("ISO", "ISO %iso"),
	("Exposure bias", "%expo_bias"),
	("Make", "%make"),
	("Model", "%model"),
	("Software", "%soft"),
	("Copyright", "©%copy"),
	("Focal length (35mm)", "%focal35mm"),
	("Advanced...", "advanced")
	]

completionDataDate :: [(String, String)]
completionDataDate = [
	("Date formatted in your system language", "%x"),
	("Hours and minutes", "%R"),
	("Hours, minutes and seconds", "%X"),
	("Hour of the day (24h). 00-23", "%H"),
	("Hour of the day (24h). 0-23", "%k"),
	("Hour of half-day (12h). 01-12", "%I"),
	("Hour of half-day (12h). 1-12", "%l"),
	("Minute of hour, 00-59", "%M"),
	("Second of minute, 00-60", "%S"),
	("Year", "%Y"),
	("Year of century, 00-99", "%y"),
	("Month name, long form, January-December", "%B"),
	("Month name, short form, Jan-Dec", "%b"),
	("Month of year, 01-12", "%m"),
	("Day of month, 01-31", "%d"),
	("Day of month, 1-31", "%e"),
	("Day of week, short form, Sun-Sat", "%a"),
	("Day of week, long form, Sunday-Saturday", "%A")
	]

data CompletionRecordType = NormalCompletion | DateCompletion
	deriving (Eq, Show)

data CompletionRecord = CompletionRecord
	{
		complRecordType :: CompletionRecordType,
		complRecordDesc :: String,
		complRecordValue :: String
	} deriving Show

isDateRecord :: CompletionRecord -> Bool
isDateRecord (CompletionRecord t _ _) = t == DateCompletion

showCustomContentsDialog :: WindowClass a => a -> Builder -> Model DisplayItem -> IO ()
showCustomContentsDialog parent builder displayItemModel = do
	dialog <- builderGetObject builder castToDialog "customContentsDialog"
	set dialog [windowTransientFor := parent]
	customContentsEntry <- builderGetObject builder castToEntry "customContentsEntry"
	curContents <- liftM itemContents $ readModel displayItemModel

	defaultDisplayItem <- readModel displayItemModel
	parseResultLabel <- builderGetObject builder castToLabel "parseResultLabel"
	customContentsEntry `on` editableChanged $ do
		text <- entryGetText customContentsEntry
		labelSetMarkup parseResultLabel $ case parseFormat text of
			Right _ -> getTextToRender (defaultDisplayItem {itemContents = text}) fakeImageInfo
			Left _ -> "<span color='red'><b>Incorrect syntax</b></span>"
	entrySetText customContentsEntry curContents

	completion <- entryCompletionNew
	let completionData = fmap (uncurry $ CompletionRecord NormalCompletion) contentsComboData 
				++ [CompletionRecord NormalCompletion "% sign" "%%"] 
				++ fmap (uncurry $ CompletionRecord DateCompletion) completionDataDate
				++ [CompletionRecord DateCompletion "% sign" "%%"] 
	completionStore <- listStoreNew completionData
	entryCompletionSetModel completion $ Just completionStore
	cellValue <- cellRendererTextNew
	cellLayoutPackStart completion cellValue True
	cellLayoutSetAttributes completion cellValue completionStore
		(\val -> [cellText := complRecordDesc val])
	cellDesc <- cellRendererTextNew
	cellLayoutPackStart completion cellDesc True
	cellLayoutSetAttributes completion cellDesc completionStore
		(\val -> [cellText := complRecordValue val])
	entryCompletionSetMinimumKeyLength completion 1
	entryCompletionSetMatchFunc completion $
		customContentsCompletionCb completionData customContentsEntry
	entrySetCompletion customContentsEntry completion
	completion `on` matchSelected $ \_ iter -> do
		let candidate = completionData !! listStoreIterToIndex iter
		textSoFar <- entryGetText customContentsEntry
		cursorPosBefore <- get customContentsEntry entryCursorPosition
		let (beforeCursor, afterCursor) = splitAt cursorPosBefore textSoFar
		textWhichGotCompleted <- liftM fromJust $ textBeforeCursorFromSymbol "%" customContentsEntry
		let lengthBeforeCompletion = length beforeCursor - length textWhichGotCompleted
		let newText = take lengthBeforeCompletion textSoFar
			++ complRecordValue candidate ++ afterCursor
		entrySetText customContentsEntry newText
		let newPos = lengthBeforeCompletion + length (complRecordValue candidate)
		editableSetPosition customContentsEntry newPos
		return True

	dialogRun dialog
	widgetHide dialog

textBeforeCursorFromSymbol :: String -> Entry -> IO (Maybe String)
textBeforeCursorFromSymbol symbol entry = do
	cursorPos <- get entry entryCursorPosition
	typed <- liftM (take cursorPos) $ entryGetText entry
	return $ find (isPrefixOf symbol) $ tail $ reverse $ tails typed

customContentsCompletionCb :: [CompletionRecord] -> Entry -> String -> TreeIter -> IO Bool
customContentsCompletionCb completionModel entry _ iter = do
	let candidate = completionModel !! listStoreIterToIndex iter
	beforePercent <- textBeforeCursorFromSymbol "%" entry
	beforeDate <- textBeforeCursorFromSymbol "%date{" entry
	let inDateContext = isJust beforeDate && (not $ "}" `isInfixOf` (fromJust beforeDate))
	case beforePercent of
		Nothing -> return False
		Just fromPercent | inDateContext ->
			return $ isDateRecord candidate && fromPercent `isPrefixOf` complRecordValue candidate
		Just fromPercent -> 
			return $ (not $ isDateRecord candidate) && fromPercent `isPrefixOf` complRecordValue candidate

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

updateConfFromModel :: IORef Conf -> ListModel DisplayItem -> ListModel TextStyle -> IO Conf
updateConfFromModel latestConfig displayItemsModel textStylesModel = do
	conf <- readIORef latestConfig
	curDisplayItems <- liftIO $ readListModel displayItemsModel >>= mapM readModel
	let conf1 = setSetting conf displayItems curDisplayItems
	curTextStyles <- liftIO $ readListModel textStylesModel >>= mapM readModel
	let conf2 = setSetting conf1 textStyles curTextStyles
	return conf2

data AspectRatio = FourThree
	| ThreeTwo

getHeightMultiplier :: ComboBox -> IO Double
getHeightMultiplier aspectRatioCombo = do
	v <- comboBoxGetActive aspectRatioCombo
	return $ case v of
		0 -> 3/4
		_ -> 2/3

getDisplayItemsStyles :: ListModel DisplayItem -> ListModel TextStyle -> IO [(DisplayItem, TextStyle)]
getDisplayItemsStyles displayItemsModel textStylesModel = do
		displayItemsV <- readListModel displayItemsModel >>= mapM readModel
		allTextStylesV <- readListModel textStylesModel >>= mapM readModel
		let textStyleById sId = find ((==sId) . styleId) allTextStylesV
		let textStylesV = fmap (fromJust . textStyleById  . textStyleId) displayItemsV
		return $ zip displayItemsV textStylesV

drawImageLayout :: DrawingArea -> ComboBox -> ListModel DisplayItem -> ListModel TextStyle -> PangoLayout -> PangoContext -> Render ()
drawImageLayout drawingArea aspectRatioCombo displayItemsModel textStylesModel text ctxt = do
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

	displayItemsStylesInfo <- liftIO $ getDisplayItemsStyles displayItemsModel textStylesModel
	renderFrame (floor effectiveW) (floor effectiveH) fakeImageInfo text ctxt displayItemsStylesInfo
	restore
	return ()

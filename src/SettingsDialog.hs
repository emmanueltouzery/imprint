{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module SettingsDialog where

import Graphics.UI.Gtk hiding (styleSet, rectangle)
import Graphics.Rendering.Cairo hiding (width, height, x)
import Data.IORef
import Data.AppSettings (getSetting', setSetting, Conf, DefaultConfig(..))
import Control.Monad (liftM, when, void)
import Control.Applicative
import Data.Maybe (fromJust, isNothing)
import Data.List
import qualified Data.Map as Map

import TextStylesSettings
import Settings
import FrameRenderer
import GtkViewModel
import Helpers
import CustomContentsDialog
import SettingsWindowData

prepareSettingsDialog :: Builder -> IORef Conf -> IO Dialog
prepareSettingsDialog builder latestConfig = do
	ctxt <- cairoCreateContext Nothing -- TODO creating cairo ctxt all over the place...

	startConf <- readIORef latestConfig
	displayItemsModel <- makeListModel $ getSetting' startConf displayItems
	textStylesModel <- makeListModel $ getSetting' startConf textStyles

	let getCurItem = fromJust <$> listModelGetCurrentItem displayItemsModel

	settingsDialog <- builderGetObject builder castToDialog "main_settings_dialog"
	imageLayout <- builderGetObject builder castToDrawingArea "image_layout"
	
	aspectRatioCombo <- builderGetObject builder castToComboBox "aspect_ratio_combo"
	comboBoxSetActive aspectRatioCombo 0 -- needed on windows
	aspectRatioCombo `on` changed $ widgetQueueDraw imageLayout

	defaultSettingsButton <- builderGetObject builder castToButton "default_settings_button"
	defaultSettingsButton `on` buttonActivated $ resetToDefaults displayItemsModel textStylesModel

	-- TODO turns out too small, workaround, forced height request of 30px
	-- in the gtkbuilder file...
	contentsCombo <- builderGetObject builder castToComboBox "contentscombo"
	cId <- newIORef Nothing
	let comboContentsBindInfo = ComboBindInfo
		{
			comboWidget = contentsCombo,
			comboValues = contentsComboData,
			comboExtraValues = [__ "Advanced"],
			defaultIndex = length contentsComboData,
			comboConnectId = cId
		}

	textLayout <- layoutEmpty ctxt
	textLayout `layoutSetText` "2014-04-01"
	imageLayout `on` draw $
		drawImageLayout imageLayout aspectRatioCombo displayItemsModel textStylesModel textLayout ctxt

	textStylePreview <- builderGetObject builder castToDrawingArea "textstylepreview"

	pickTextStyleBtn <- builderGetObject builder castToButton "picktextstylebtn"
	pickTextStyleBtn `on` buttonActivated $ do
		showTextStyleListDialog builder displayItemsModel textStylesModel settingsDialog
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

	maxWidthScale <- builderGetObject builder castToScale "maxWidthScale"
	let maxWidthScaleBindInfo = horMarginRangeBindInfo
		{ range = maxWidthScale, lowerV = 10, upperV = 100 }

	text <- layoutEmpty ctxt
	text `layoutSetText` "2014-04-01"
	textStylePreview `on` draw $ do
		cTextStyle <- liftIO $ getDisplayItemTextStyle displayItemsModel textStylesModel
		fontSize <- liftIO $ setFontSizeForWidget ctxt text textStylePreview cTextStyle
		renderText text ctxt cTextStyle $ fromIntegral fontSize

	displayItemPositionCombo <- builderGetObject builder castToComboBox "displayItemPositionCombo"

	positionDeleteButton <- builderGetObject builder castToButton "positionDeleteButton"
	positionDeleteButton `on` buttonActivated $
		handlePositionDelete settingsDialog displayItemsModel displayItemPositionCombo

	builderHolder <- getBuilderHolder builder

	contentsAdvancedEdit <- builderGetObject builder castToButton "contentsAdvancedEdit"
	contentsAdvancedEdit `on` buttonActivated $
		getCurItem >>= showCustomContentsDialog settingsDialog builderHolder
	
	let showHideAdvancedEdit = do
		idx <- comboBoxGetActive contentsCombo
		-- is advanced picked in the contents combo?
		if idx == defaultIndex comboContentsBindInfo
			then widgetShow contentsAdvancedEdit
			else widgetHide contentsAdvancedEdit

	contentsComboChangedConnectId <- newIORef Nothing

	addListModelCurrentItemObserver displayItemsModel $ \currentDisplayItemModel -> do
		comboConnId <- readIORef contentsComboChangedConnectId
		whenIsJust comboConnId signalDisconnect
		bindModel currentDisplayItemModel marginXFromWidthL horMarginRangeBindInfo
		bindModel currentDisplayItemModel textSizeFromWidthL scaleRangeBindInfo
		bindModel currentDisplayItemModel marginYFromWidthL verMarginRangeBindInfo
		bindModel currentDisplayItemModel maxWidthFromWidthL maxWidthScaleBindInfo
		bindModel currentDisplayItemModel itemContentsL comboContentsBindInfo
		let redraw = do
			widgetQueueDraw imageLayout
			widgetQueueDraw textStylePreview
			showHideAdvancedEdit
		redraw
		-- this is so that we redraw when the current item is modified.
		addModelObserver currentDisplayItemModel $ const redraw

		-- this at the end so that it does not trigger
		-- when we open the dialog, but only in case
		-- of an action from a real user.
		newComboConnectId <- contentsCombo `on` changed $ do
			comboPos <- comboBoxGetActive contentsCombo
			when (comboPos == length contentsComboData) $
				getCurItem >>= showCustomContentsDialog settingsDialog builderHolder
		modifyIORef contentsComboChangedConnectId $ const $ Just newComboConnectId

	readListModel displayItemsModel >>= listModelSetCurrentItem displayItemsModel . head

	position <$> (getCurItem >>= readModel) >>= comboBoxSelectPosition displayItemPositionCombo
	displayItemPositionCombo `on` changed $
		changeDisplayItemPosition settingsDialog displayItemPositionCombo displayItemsModel

	settingsOkBtn <- builderGetObject builder castToButton "settingsOk"
	settingsOkBtn `on` buttonActivated $ do
		updatedConf <- liftIO $ updateConfFromModel latestConfig displayItemsModel textStylesModel
		modifyIORef latestConfig $ const updatedConf
		saveSettings updatedConf
		widgetHide settingsDialog

	settingsCancelBtn <- builderGetObject builder castToButton "settingsCancel"
	settingsCancelBtn `on` buttonActivated $ widgetHide settingsDialog

	windowSetDefaultSize settingsDialog 600 500
	return settingsDialog
	--prepareTextStylePreview builder latestConfig

handlePositionDelete :: Dialog -> ListModel DisplayItem -> ComboBox -> IO ()
handlePositionDelete settingsDialog displayItemsModel displayItemPositionCombo = do
	confirm <- dialogYesNo settingsDialog $ __ "Are you sure to remove the display item?"
	when confirm $ do
		itemToRemove <- fromJust <$> listModelGetCurrentItem displayItemsModel 
		newCurItem <- find (/= itemToRemove) <$> readListModel displayItemsModel
		case newCurItem of
			Nothing -> displayError settingsDialog $ __ "Can't delete the last display item"
			Just newItem -> do
				listModelSetCurrentItem displayItemsModel newItem
				newPos <- position <$> readModel newItem
				comboBoxSelectPosition displayItemPositionCombo newPos
				listModelRemoveItem displayItemsModel itemToRemove

comboIndexes :: [(Int, ItemPosition)]
comboIndexes = [(0, TopLeft), (1, TopCenter), (2, TopRight),
		(3, BottomLeft), (4, BottomCenter), (5, BottomRight)]

changeDisplayItemPosition :: Dialog -> ComboBox -> ListModel DisplayItem -> IO ()
changeDisplayItemPosition parent displayItemPositionCombo displayItemsModel = do
	selectedPosition <- comboBoxGetPosition displayItemPositionCombo
	itemsModels <- readListModel displayItemsModel
	items <- mapM readModel itemsModels
	case find ((==selectedPosition) . position . snd) (zip itemsModels items) of
		Nothing -> do
			isCreate <- offerCreate parent selectedPosition displayItemsModel
			when (not isCreate) $ do
				-- user refused to create, reset the combo to the previous position.
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

offerCreate :: Dialog -> ItemPosition -> ListModel DisplayItem -> IO Bool
offerCreate parent itemPosition displayItemsModel = do
	isCreate <- dialogYesNo parent $ __ "There is no item to display at that position. Create one?"
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
	w <- liftIO $ fromIntegral <$> widgetGetAllocatedWidth drawingArea
	h <- liftIO $ fromIntegral <$> widgetGetAllocatedHeight drawingArea

	let (effectiveW, effectiveH) = if w*heightMultiplier < h
		then (w, w*heightMultiplier)
		else (h/heightMultiplier, h)

	let top = (h-effectiveH)/2

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

resetToDefaults :: ListModel DisplayItem -> ListModel TextStyle -> IO ()
resetToDefaults displayItemsModel textStylesModel = do
	-- Add text styles present in default, but not currently (by id)
	-- and overwrite those with same id from default
	curTextStyleModelsByStyleId <- addOrOverwriteAll textStylesModel styleId defaultTextStyles
	-- Add contents in new positions (topleft itd)
	-- Overwrite contents in same positions
	curDisplayItemModelsByPosition <- addOrOverwriteAll displayItemsModel position defaultDisplayItems
	-- Remove extra contents
	mapM_ (removeIfNotPresentByDefault displayItemsModel position defaultDisplayItems) curDisplayItemModelsByPosition
	-- Remove extra styles
	mapM_ (removeIfNotPresentByDefault textStylesModel styleId defaultTextStyles) curTextStyleModelsByStyleId
	where
		(DefaultConfig defaultConf) = getAllSettings
		defaultDisplayItems = getSetting' defaultConf displayItems
		defaultTextStyles = getSetting' defaultConf textStyles

addOrOverwriteAll :: (Eq a, Ord b) => ListModel a -> (a -> b) -> [a] -> IO [(b, Model a)]
addOrOverwriteAll listModel getKey defaultList = do
	-- Add items present in default, but not currently (by key)
	-- and overwrite those with same key from default
	listModelByKey <- readListModel listModel >>= makeHashByKey getKey
	mapM_ (addOrOverwrite listModel listModelByKey getKey) defaultList
	-- set the current item to the first one from the default config.
	listModelFind (== head defaultList) listModel >>= listModelSetCurrentItem listModel . fromJust
	return $ Map.toList listModelByKey

-- TODO must be able to make this look nicer...?
makeHashByKey :: Ord b => (a -> b) -> [Model a] -> IO (Map.Map b (Model a))
makeHashByKey getKey = liftM Map.fromList . mapM (\x -> flip (,) x . getKey <$> readModel x)

addOrOverwrite :: Ord b => ListModel a -> Map.Map b (Model a) -> (a -> b) -> a -> IO ()
addOrOverwrite listModel modelItemsByKey getKey newItem =
	case Map.lookup (getKey newItem) modelItemsByKey of
		Nothing -> makeModel newItem >>= listModelAddItem listModel
		Just curModel -> void (modifyModel curModel $ const newItem)

removeIfNotPresentByDefault :: (Eq a, Eq b) => ListModel a -> (a -> b) -> [a] -> (b, Model a) -> IO ()
removeIfNotPresentByDefault listModel getKey defaultList (key, value) =
	when (isNothing $ find ((==key) . getKey) defaultList) $
		listModelRemoveItem listModel value

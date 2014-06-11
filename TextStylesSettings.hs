{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module TextStylesSettings where

import Graphics.Rendering.Cairo hiding (width, height, x)
import Graphics.Rendering.Cairo.SVG
import Graphics.UI.Gtk hiding (styleSet)
import Data.Maybe (fromJust, isJust)
import Data.IORef
import Control.Monad (when, liftM)
import Data.AppSettings (getSetting', Conf, setSetting)
import Data.List
import Control.Lens hiding (set)

import Settings
import GtkViewModel
import FrameRenderer
import Helpers

minFontSize :: Int
minFontSize = 5

getDisplayItemTextStyle :: Conf -> ItemPosition -> TextStyle
getDisplayItemTextStyle conf itemPosition = case find ((==selectedStyleId) . styleId) allStyles of
		Nothing -> error $ "Can't find text style of id " ++ show selectedStyleId
		Just x -> x
	where
		allStyles = getSetting' conf textStyles
		selectedStyleId = textStyleId displayItem
		displayItem = fromJust $ getDisplayItem conf itemPosition

-- TODO same as getDisplayItemTextStyle return without the maybe
-- but trigger a error with a debuggable error message if it's not there.
-- (but in both cases don't use case but some Data.Maybe function)
getDisplayItem :: Conf -> ItemPosition -> Maybe DisplayItem
getDisplayItem conf itemPosition = find ((==itemPosition) . position) $ getSetting' conf displayItems

-- here is the reason why we work on ItemPosition and not on DisplayItem...
-- we are a settings dialog. And so we will modify the display item...
-- if nothing else we'll change its textStyleId.
-- And once modified if I search for the item i'll fail to find
-- it because it was modified.
-- So I rely on the fact that there is only one display item for
-- one ItemPosition. So I find the display item by the position
-- and I don't have to worry whether the item was modified or not,
-- I know I'll find it.
showTextStyleListDialog :: Builder -> Conf -> ItemPosition -> Window -> IO Conf
showTextStyleListDialog builder conf itemPosition parent = do
	curConfig <- newIORef conf
	activeItemSvg <- svgNewFromFile "active_item.svg"
	dialog <- builderGetObject builder castToDialog "dialog1"
	stylesVbox <- builderGetObject builder castToBox "stylesVbox"
	containerForeach stylesVbox (\w -> containerRemove stylesVbox w)
	textStyleDialogInfo <- prepareTextStyleDialog builder $
		getDisplayItemTextStyle conf itemPosition
	ctxt <- cairoCreateContext Nothing

	let styles = getSetting' conf textStyles
	let styleIds = fmap styleId styles
	let styleGettersSetters = fmap (getStyleGetterSetter stylesVbox curConfig) styleIds

	textStyleListBtnCancel <- builderGetObject builder castToButton "textStyleListBtnCancel"
	textStyleListBtnCancel `on` buttonActivated $ do
		dialogResponse dialog ResponseCancel
		widgetHide dialog

	textStyleListBtnOk <- builderGetObject builder castToButton "textStyleListBtnOk"
	textStyleListBtnOk `on` buttonActivated $ do
		dialogResponse dialog ResponseOk
		widgetHide dialog

	mapM_ (uncurry $ vboxAddStyleItem dialog stylesVbox ctxt activeItemSvg textStyleDialogInfo curConfig itemPosition) styleGettersSetters
	windowSetDefaultSize dialog 600 500
	set dialog [windowTransientFor := parent]
	resp <- dialogRun dialog
	widgetHide dialog
	case resp of
		ResponseOk -> do
			readIORef curConfig
		_ -> do
			return conf -- user pressed cancel

getStyleGetterSetter :: Box -> IORef Conf -> Int -> (Conf->TextStyle, TextStyle -> IO ())
getStyleGetterSetter stylesVbox latestConfig cStyleId = (getStyleById cStyleId, updateStyle latestConfig stylesVbox cStyleId)

getStyleById :: Int -> Conf -> TextStyle
getStyleById cStyleId conf = fromJust $ find ((==cStyleId) . styleId) allStyles
	where allStyles = getSetting' conf textStyles

updateStyle :: IORef Conf -> Box -> Int -> TextStyle -> IO ()
updateStyle latestConfig stylesVbox cStyleId newStyle = do
	updateConfig latestConfig $ \c -> do
		let allStyles = getSetting' c textStyles
		let styleIdx = fromJust $ findIndex ((==cStyleId) . styleId) allStyles
		let newConf = setSetting c textStyles $ allStyles & ix styleIdx .~ newStyle
		widgetQueueDraw stylesVbox
		return newConf

prepareTextStyleDrawingArea :: PangoContext -> PangoLayout -> DrawingArea -> IO ()
prepareTextStyleDrawingArea ctxt text drawingArea = do
	drawingArea `on` configureEvent $ do
		-- widget resize
		liftIO $ setFontSizeForWidget ctxt text drawingArea
		return True
	return ()

-- Maybe could use Gtk signals for the styleUpdatedCallback,
-- but don't know how/whether it's possible.
-- TODO too many parameters, and way too long!!!
vboxAddStyleItem :: Dialog -> Box -> PangoContext -> SVG -> TextStyleDialogInfo -> IORef Conf -> ItemPosition
		-> (Conf->TextStyle) -> (TextStyle -> IO ()) -> IO ()
vboxAddStyleItem parent box ctxt activeItemSvg textStyleDialogInfo latestConfig itemPosition confTextStyleGetter styleUpdatedCallback = do
	text <- layoutEmpty ctxt
	text `layoutSetText` "2014-04-01"

	checkbox <- drawingAreaNew
	widgetSetSizeRequest checkbox 50 100
 	let cbH = snd $ svgGetSize activeItemSvg
	let cbYtop = 100 `div` 2 - cbH `div` 2

	drawingArea <- drawingAreaNew
	widgetSetSizeRequest drawingArea 450 100

	prepareTextStyleDrawingArea ctxt text drawingArea
	drawingArea `on` draw $ do
		conf <- liftIO $ readIORef latestConfig
		let cTextStyle = confTextStyleGetter conf
		liftIO $ do
			setFontSizeForWidget ctxt text drawingArea
		renderText text ctxt cTextStyle

	checkbox `on` draw $ do
		conf <- liftIO $ readIORef latestConfig
		let cTextStyle = confTextStyleGetter conf
		let selectedStyleId = getDisplayItemTextStyle conf itemPosition
		when (selectedStyleId == cTextStyle) $ do
			translate 0 $ fromIntegral cbYtop
			svgRender activeItemSvg >> return ()
			translate 0 $ (-fromIntegral cbYtop)

	let styleSelectCb = do
		liftIO $ updateConfig latestConfig $ changeSelectedConfig confTextStyleGetter box itemPosition
		return True

	widgetAddEvents checkbox [ButtonPressMask, ButtonReleaseMask]
	widgetAddEvents drawingArea [ButtonPressMask, ButtonReleaseMask]
	checkbox `on` buttonReleaseEvent $ styleSelectCb
	drawingArea `on` buttonReleaseEvent $ styleSelectCb

	-- TODO move to GTK/glade widget templates?
	hbox <- hBoxNew False 0
	boxPackStart hbox checkbox PackNatural 0
	boxPackStart hbox drawingArea PackNatural 0
	vbtnBox <- vButtonBoxNew
	copyBtn <- prepareButton stockCopy
	copyBtn `on` buttonActivated $ do
		conf <- readIORef latestConfig
		let newStyle = styleIdL .~ getNewStyleId conf $ confTextStyleGetter conf
		let styles = getSetting' conf textStyles
		let newConf = setSetting conf textStyles $ styles ++ [newStyle]
		Settings.saveSettings newConf
		writeIORef latestConfig newConf
		let (styleGet, styleSet) = getStyleGetterSetter box latestConfig $ styleId newStyle
		vboxAddStyleItem parent box ctxt activeItemSvg textStyleDialogInfo latestConfig itemPosition styleGet styleSet
		
	containerAdd vbtnBox copyBtn
	editBtn <- prepareButton stockEdit
	editBtn `on` buttonActivated $ do
		conf <- readIORef latestConfig
		showTextStyleDialog parent textStyleDialogInfo (confTextStyleGetter conf) styleUpdatedCallback
 	containerAdd vbtnBox editBtn
	deleteBtn <- prepareButton stockDelete
	deleteBtn `on` buttonActivated $ do
		userConfirms <- dialogYesNo parent "Sure to delete the text style?"
		when userConfirms $ updateConfig latestConfig $ removeTextStyle parent confTextStyleGetter box
		
	containerAdd vbtnBox deleteBtn
	boxPackStart hbox vbtnBox PackNatural 0

	boxPackStart box hbox PackNatural 0
	widgetShowAll hbox

changeSelectedConfig :: (Conf->TextStyle) -> Box -> ItemPosition -> Conf -> IO Conf
changeSelectedConfig confTextStyleGetter box itemPosition conf = do
	let clickedStyleId = styleId $ confTextStyleGetter conf
	let displayItemsV = getSetting' conf displayItems
	let displayItem = fromJust $ find ((==itemPosition) . position) displayItemsV
	-- doesn't matter if we change the order, could be a hash
	-- key = item position, value = the rest.
	let newDisplayItems = delete displayItem displayItemsV ++ [displayItem { textStyleId = clickedStyleId }]
	let newConf = setSetting conf displayItems newDisplayItems
	widgetQueueDraw box
	return newConf

removeTextStyle :: Dialog -> (Conf->TextStyle) -> Box -> Conf -> IO Conf
removeTextStyle parent confTextStyleGetter box conf = do
	let styleIdToRemove = styleId $ confTextStyleGetter conf
	let displayItemsV = getSetting' conf displayItems
	let usedTextStyleIds = fmap textStyleId displayItemsV
	if styleIdToRemove `elem` usedTextStyleIds
		then do
			displayError parent "Cannot delete the selected text style"
			return conf
		else do
			let cStyles = getSetting' conf textStyles
			let styleIdx = fromJust $ findIndex ((==styleIdToRemove) . styleId) cStyles
			let newConf = setSetting conf textStyles $ filter ((/=styleIdToRemove) . styleId) cStyles
			boxWidgets <- containerGetChildren box
			containerRemove box $ boxWidgets !! styleIdx
			return newConf

getNewStyleId :: Conf -> Int
getNewStyleId conf = 1 + maximum' existingIds
	where existingIds = fmap styleId (getSetting' conf textStyles)

maximum' :: [Int] -> Int
maximum' [] = 0
maximum' x = maximum x

prepareButton :: StockId -> IO Button
prepareButton stockId = do
	btn <- buttonNew
	img <- imageNewFromStock stockId IconSizeSmallToolbar
	buttonSetImage btn img
	buttonSetRelief btn ReliefNone
	return btn

data TextStyleDialogInfo = TextStyleDialogInfo (Model TextStyle) Button Dialog (IORef (Maybe (ConnectId Button)))

prepareTextStyleDialog :: Builder -> TextStyle -> IO TextStyleDialogInfo
prepareTextStyleDialog builder textStyle = do
	ctxt <- cairoCreateContext Nothing
	text <- layoutEmpty ctxt
	text `layoutSetText` "2014-04-01"

	textStyleModel <- makeModel textStyle

	dialog <- builderGetObject builder castToDialog "settings_dialog"
	textPreview <- builderGetObject builder castToDrawingArea "textPreview"

	prepareTextStyleDrawingArea ctxt text textPreview
	textPreview `on` draw $ do
		cTextStyle <- liftIO $ readModel textStyleModel
		renderText text ctxt cTextStyle

	builderGetObject builder castToColorButton "fillColor" >>= bindModel textStyleModel textFillL
	builderGetObject builder castToColorButton "strokeColor" >>= bindModel textStyleModel textStrokeL

	borderScale <- builderGetObject builder castToScale "borderScale"
	let rangeBindInfo = RangeBindInfo
		{
			range = borderScale,
			lowerV = 0,
			upperV = 22,
			stepIncr = 2,
			pageIncr = 1,
			pageSize = 1
		}
	bindModel textStyleModel strokeHeightRatioL rangeBindInfo

	fontButton <- builderGetObject builder castToFontButton "fontButton"
	bindModel textStyleModel fontNameL fontButton

	-- TODO we're changing the font when any setting changes.
	-- no need to recompute the font when the colors are changed
	-- for instance.
	addModelObserver textStyleModel $ \_ -> do
		setFontSizeForWidget ctxt text textPreview
		-- the following however is always needed.
		widgetQueueDraw textPreview

	textStyleBtnCancel <- builderGetObject builder castToButton "textStyleBtnCancel"
	textStyleBtnCancel `on` buttonActivated $ widgetHide dialog

	textStyleBtnOk <- builderGetObject builder castToButton "textStyleBtnOk"

	okSignalRef <- newIORef Nothing

	return $ TextStyleDialogInfo textStyleModel textStyleBtnOk dialog okSignalRef

showTextStyleDialog :: Dialog -> TextStyleDialogInfo -> TextStyle -> (TextStyle -> IO ()) -> IO ()
showTextStyleDialog parent (TextStyleDialogInfo curTextStyle textStyleBtnOk dialog okSigRef) textStyle updateCallback = do
	modifyModel curTextStyle $ const textStyle
	okSig <- readIORef okSigRef
	when (isJust okSig) $ signalDisconnect $ fromJust okSig
	newOkSig <- textStyleBtnOk `on` buttonActivated $ do
		readModel curTextStyle >>= updateCallback
		widgetHide dialog
	writeIORef okSigRef (Just newOkSig)
	
	windowSetDefaultSize dialog 450 300
	set dialog [windowTransientFor := parent]
	dialogRun dialog
	widgetHide dialog
	return ()

setFontSizeForWidget :: WidgetClass a => PangoContext -> PangoLayout -> a -> IO ()
setFontSizeForWidget ctxt text widget = liftIO $ do
	w <- widgetGetAllocatedWidth widget
	h <- widgetGetAllocatedHeight widget
	setFontSizeForBoundingBox ctxt text minFontSize w h

setFontSizeForBoundingBox :: PangoContext -> PangoLayout -> Int -> Int -> Int -> IO ()
setFontSizeForBoundingBox ctxt text fontSize maxWidth maxHeight = do
	contextSetFontSize ctxt $ fromIntegral fontSize
	-- the next two lines are needed on windows.
	fnt <- contextGetFontDescription ctxt
	layoutSetFontDescription text $ Just fnt
	(Rectangle _ _ rWidth rHeight) <- liftM snd $ layoutGetPixelExtents text
	if rWidth < maxWidth && rHeight < maxHeight
		then setFontSizeForBoundingBox ctxt text (fontSize+1) maxWidth maxHeight
		else contextSetFontSize ctxt $ fromIntegral $ fontSize-1

updateConfig :: IORef Conf -> (Conf -> IO Conf) -> IO ()
updateConfig latestConfig newConfigMaker = do
	conf <- readIORef latestConfig
	conf' <- newConfigMaker conf
	saveSettings conf'
	writeIORef latestConfig conf'

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module TextStylesSettings where

import Graphics.Rendering.Cairo hiding (width, height, x)
import Graphics.Rendering.Cairo.SVG
import Graphics.UI.Gtk hiding (styleSet)
import Data.Maybe (fromJust)
import Control.Monad (when, liftM)
import Data.List
import Control.Lens hiding (set)

import Paths_imprint (getDataFileName)

import Settings hiding (saveSettings)
import GtkViewModel
import FrameRenderer
import Helpers

minFontSize :: Int
minFontSize = 5

getDisplayItemTextStyleModel :: ListModel DisplayItem -> ListModel TextStyle -> IO (Model TextStyle)
getDisplayItemTextStyleModel displayItemsModel textStylesModel = do
	allStylesModels <- readListModel textStylesModel
	allStyles <- readListModel textStylesModel >>= mapM readModel
	displayItem <- getCurrentDisplayItem displayItemsModel
	let selectedStyleId = textStyleId displayItem
	case findIndex ((==selectedStyleId) . styleId) allStyles of
		Nothing -> error $ "Can't find text style of id " ++ show selectedStyleId
		Just idx -> return $ allStylesModels !! idx

getDisplayItemTextStyle :: ListModel DisplayItem -> ListModel TextStyle -> IO TextStyle
getDisplayItemTextStyle displayItemsModel textStylesModel =
	getDisplayItemTextStyleModel displayItemsModel textStylesModel >>= readModel

getCurrentDisplayItem :: ListModel DisplayItem -> IO DisplayItem
getCurrentDisplayItem displayItemsModel = listModelGetCurrentItem displayItemsModel >>= readModel . fromJust

-- here is the reason why we work on ItemPosition and not on DisplayItem...
-- we are a settings dialog. And so we will modify the display item...
-- if nothing else we'll change its textStyleId.
-- And once modified if I search for the item i'll fail to find
-- it because it was modified.
-- So I rely on the fact that there is only one display item for
-- one ItemPosition. So I find the display item by the position
-- and I don't have to worry whether the item was modified or not,
-- I know I'll find it.
showTextStyleListDialog :: Builder -> ListModel DisplayItem -> ListModel TextStyle -> Dialog -> IO ()
showTextStyleListDialog builder displayItemsModel textStylesModel parent = do
	activeItemSvg <- getDataFileName "active_item.svg" >>= svgNewFromFile
	dialog <- builderGetObject builder castToDialog "dialog1"
	stylesVbox <- builderGetObject builder castToBox "stylesVbox"
	containerForeach stylesVbox (\w -> containerRemove stylesVbox w)

	-- set the current text style in the list to the text style
	-- of the current display item.
	curTextStyleModel <- getDisplayItemTextStyleModel displayItemsModel textStylesModel
	listModelSetCurrentItem textStylesModel curTextStyleModel

	textStyleDialogInfo <- readModel curTextStyleModel >>= prepareTextStyleDialog builder
	ctxt <- cairoCreateContext Nothing

	textStyleListBtnClose <- builderGetObject builder castToButton "textStyleListBtnClose"
	textStyleListBtnClose `on` buttonActivated $ widgetHide dialog

	styles <- readListModel textStylesModel
	mapM_ (vboxAddStyleItem dialog stylesVbox ctxt activeItemSvg textStyleDialogInfo displayItemsModel textStylesModel) styles
	windowSetDefaultSize dialog 600 500
	set dialog [windowTransientFor := parent]
	dialogRun dialog
	widgetHide dialog

prepareTextStyleDrawingArea :: PangoContext -> PangoLayout -> DrawingArea -> Model TextStyle -> IO ()
prepareTextStyleDrawingArea ctxt text drawingArea textStyleModel = do
	drawingArea `on` configureEvent $ do
		-- widget resize
		liftIO $ readModel textStyleModel >>= setFontSizeForWidget ctxt text drawingArea
		return True
	return ()

-- Maybe could use Gtk signals for the styleUpdatedCallback,
-- but don't know how/whether it's possible.
-- TODO too many parameters, and way too long!!!
vboxAddStyleItem :: Dialog -> Box -> PangoContext -> SVG -> TextStyleDialogInfo
		 -> ListModel DisplayItem -> ListModel TextStyle -> Model TextStyle -> IO ()
vboxAddStyleItem parent box ctxt activeItemSvg textStyleDialogInfo displayItemsModel textStylesModel curStyleModel = do
	text <- layoutEmpty ctxt
	text `layoutSetText` "2014-04-01"

	checkbox <- drawingAreaNew
	widgetSetSizeRequest checkbox 50 100
 	let cbH = snd $ svgGetSize activeItemSvg
	let cbYtop = 100 `div` 2 - cbH `div` 2

	drawingArea <- drawingAreaNew
	widgetSetSizeRequest drawingArea 450 100

	prepareTextStyleDrawingArea ctxt text drawingArea curStyleModel
	drawingArea `on` draw $ drawTextStylePreview drawingArea ctxt text curStyleModel

	checkbox `on` draw $ do
		cTextStyle <- liftIO $ readModel curStyleModel
		selectedStyleId <- liftIO $ listModelGetCurrentItem textStylesModel >>= readModel . fromJust
		when (selectedStyleId == cTextStyle) $ do
			translate 0 $ fromIntegral cbYtop
			svgRender activeItemSvg >> return ()
			translate 0 $ (-fromIntegral cbYtop)

	let styleSelectCb = liftIO $ do
		cTextStyle <- readModel curStyleModel
		curDisplayItemModel <- liftM fromJust $ listModelGetCurrentItem displayItemsModel
		modifyModel curDisplayItemModel $ \i -> i { textStyleId = styleId cTextStyle }
		listModelSetCurrentItem textStylesModel curStyleModel
		widgetQueueDraw box -- TODO maybe through listeners?
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
		cTextStyle <- readModel curStyleModel
		newStyleId <- getNewStyleId textStylesModel
		let newStyle = styleIdL .~ newStyleId $ cTextStyle
		newStyleModel <- makeModel newStyle
		listModelAddItem textStylesModel newStyleModel
		-- or register a listener on the list model and have it do that for me?
		vboxAddStyleItem parent box ctxt activeItemSvg textStyleDialogInfo displayItemsModel textStylesModel newStyleModel
		
	containerAdd vbtnBox copyBtn
	editBtn <- prepareButton stockEdit
	editBtn `on` buttonActivated $ do
		showTextStyleDialog parent textStyleDialogInfo curStyleModel
 	containerAdd vbtnBox editBtn
	deleteBtn <- prepareButton stockDelete
	deleteBtn `on` buttonActivated $ do
		userConfirms <- dialogYesNo parent "Sure to delete the text style?"
		when userConfirms $ removeTextStyle parent box textStylesModel displayItemsModel curStyleModel
		
	containerAdd vbtnBox deleteBtn
	boxPackStart hbox vbtnBox PackNatural 0

	boxPackStart box hbox PackNatural 0
	widgetShowAll hbox

drawTextStylePreview :: DrawingArea -> PangoContext -> PangoLayout -> Model TextStyle -> Render ()
drawTextStylePreview drawingArea ctxt text curStyleModel = do
	cTextStyle <- liftIO $ readModel curStyleModel
	fontSize <- liftIO $ do
		setFontSizeForWidget ctxt text drawingArea cTextStyle
	renderText text ctxt cTextStyle $ fromIntegral fontSize

removeTextStyle :: Dialog -> Box -> ListModel TextStyle -> ListModel DisplayItem -> Model TextStyle -> IO ()
removeTextStyle parent box textStylesModel displayItemsModel textStyleModel = do
	styleIdToRemove <- liftM styleId $ readModel textStyleModel
	displayItemsV <- readListModel displayItemsModel >>= mapM readModel
	let usedTextStyleIds = fmap textStyleId displayItemsV
	if styleIdToRemove `elem` usedTextStyleIds
		then displayError parent "Cannot delete the selected text style"
		else do
			cStyles <- readListModel textStylesModel
			let styleIdx = fromJust $ findIndex (==textStyleModel) cStyles
			listModelRemoveItem textStylesModel textStyleModel
			boxWidgets <- containerGetChildren box
			containerRemove box $ boxWidgets !! styleIdx

getNewStyleId :: ListModel TextStyle -> IO Int
getNewStyleId textStylesModel = do
	styles <- readListModel textStylesModel >>= mapM readModel
	return $ 1 + maximum' (fmap styleId styles)

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

data TextStyleDialogInfo = TextStyleDialogInfo (Model TextStyle) ButtonBinder Dialog

prepareTextStyleDialog :: Builder -> TextStyle -> IO TextStyleDialogInfo
prepareTextStyleDialog builder textStyle = do
	ctxt <- cairoCreateContext Nothing
	text <- layoutEmpty ctxt
	text `layoutSetText` "2014-04-01"

	textStyleModel <- makeModel textStyle

	dialog <- builderGetObject builder castToDialog "settings_dialog"
	textPreview <- builderGetObject builder castToDrawingArea "textPreview"

	prepareTextStyleDrawingArea ctxt text textPreview textStyleModel
	textPreview `on` draw $ drawTextStylePreview textPreview ctxt text textStyleModel

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

	addModelObserver textStyleModel $ \_ -> do
		--setFontSizeForWidget ctxt text textPreview
		-- the following however is always needed.
		widgetQueueDraw textPreview

	textStyleBtnCancel <- builderGetObject builder castToButton "textStyleBtnCancel"
	textStyleBtnCancel `on` buttonActivated $ widgetHide dialog

	builderHolder <- getBuilderHolder builder
	textStyleBtnOk <- builderHolderGetButtonBinder builderHolder "textStyleBtnOk"

	return $ TextStyleDialogInfo textStyleModel textStyleBtnOk dialog

showTextStyleDialog :: Dialog -> TextStyleDialogInfo -> Model TextStyle -> IO ()
showTextStyleDialog parent (TextStyleDialogInfo curTextStyle textStyleBtnOk dialog) textStyleModel = do
	readModel textStyleModel >>= modifyModel curTextStyle . const
	buttonBindCallback textStyleBtnOk $ do
		readModel curTextStyle >>= modifyModel textStyleModel . const
		widgetHide dialog
	
	windowSetDefaultSize dialog 450 300
	set dialog [windowTransientFor := parent]
	dialogRun dialog
	widgetHide dialog
	return ()

setFontSizeForWidget :: WidgetClass a => PangoContext -> PangoLayout -> a -> TextStyle -> IO Int
setFontSizeForWidget ctxt text widget textStyle = liftIO $ do
	w <- widgetGetAllocatedWidth widget
	h <- widgetGetAllocatedHeight widget
	setFontSizeForBoundingBox ctxt text minFontSize w h textStyle

setFontSizeForBoundingBox :: PangoContext -> PangoLayout -> Int -> Int -> Int -> TextStyle -> IO Int
setFontSizeForBoundingBox ctxt text fontSize maxWidth maxHeight textStyle = do
	fnt <- case fontName textStyle of
		Nothing -> contextGetFontDescription ctxt
		Just name -> fontDescriptionFromString name
	liftIO $ fontDescriptionSetSize fnt $ fromIntegral fontSize
	layoutSetFontDescription text $ Just fnt
	(Rectangle _ _ rWidth rHeight) <- liftM snd $ layoutGetPixelExtents text
	if rWidth < maxWidth && rHeight < maxHeight
		then setFontSizeForBoundingBox ctxt text (fontSize+1) maxWidth maxHeight textStyle
		else do
			layoutFnt <- liftM fromJust $ layoutGetFontDescription text
			fontDescriptionSetSize layoutFnt $ fromIntegral $ fontSize-1
			layoutSetFontDescription text $ Just layoutFnt
			return $ fontSize-1

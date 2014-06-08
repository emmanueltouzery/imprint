{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module TextStylesSettings where

import Graphics.Rendering.Cairo hiding (width, height, x)
import Graphics.Rendering.Cairo.SVG
import Graphics.UI.Gtk hiding (styleSet)
import Data.Maybe (fromJust, isJust)
import Data.IORef
import Control.Monad (when, liftM)
import Data.AppSettings (getSetting', Conf, setSetting, GetSetting(..))
import Data.List
import Control.Lens hiding (set)

import Helpers
import Settings
import GtkMvvm

minFontSize :: Int
minFontSize = 5

getSelectedTextStyle :: GetSetting -> TextStyle
getSelectedTextStyle (GetSetting getSetting) = case find ((==selectedStyleId) . styleId) allStyles of
		Nothing -> error $ "Can't find text style of id " ++ show selectedStyleId
		Just x -> x
	where
		allStyles = getSetting textStyles
		selectedStyleId = getSetting selectedTextStyleId

showTextStyleListDialog :: Builder -> IORef Conf -> Window -> IO ()
showTextStyleListDialog builder latestConfig parent = do
	activeItemSvg <- svgNewFromFile "active_item.svg"
	dialog <- builderGetObject builder castToDialog "dialog1"
	stylesVbox <- builderGetObject builder castToBox "stylesVbox"
	containerForeach stylesVbox (\w -> containerRemove stylesVbox w)
	conf <- readIORef latestConfig
	textStyleDialogInfo <- prepareTextStyleDialog builder $
		getSelectedTextStyle (GetSetting $ getSetting' conf)
	ctxt <- cairoCreateContext Nothing

	let styles = getSetting' conf textStyles
	let styleIds = fmap styleId styles
	let styleGettersSetters = fmap (getStyleGetterSetter stylesVbox latestConfig) styleIds

	textStyleListBtnCancel <- builderGetObject builder castToButton "textStyleListBtnCancel"
	textStyleListBtnCancel `on` buttonActivated $ widgetHide dialog

	mapM_ (uncurry $ vboxAddStyleItem dialog stylesVbox ctxt activeItemSvg textStyleDialogInfo latestConfig) styleGettersSetters
	windowSetDefaultSize dialog 600 500
	set dialog [windowTransientFor := parent]
	dialogRun dialog
	widgetHide dialog
	return ()

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
vboxAddStyleItem :: Dialog -> Box -> PangoContext -> SVG -> TextStyleDialogInfo -> IORef Conf -> (Conf->TextStyle)
		-> (TextStyle -> IO ()) -> IO ()
vboxAddStyleItem parent box ctxt activeItemSvg textStyleDialogInfo latestConfig confTextStyleGetter styleUpdatedCallback = do
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
			updateFontFromTextStyle ctxt cTextStyle
			setFontSizeForWidget ctxt text drawingArea
		renderText text cTextStyle

	checkbox `on` draw $ do
		conf <- liftIO $ readIORef latestConfig
		let cTextStyle = confTextStyleGetter conf
		let selectedStyleId = getSetting' conf selectedTextStyleId
		when (selectedStyleId == styleId cTextStyle) $ do
			translate 0 $ fromIntegral cbYtop
			svgRender activeItemSvg >> return ()
			translate 0 $ (-fromIntegral cbYtop)

	let styleSelectCb = do
		liftIO $ updateConfig latestConfig $ changeSelectedConfig confTextStyleGetter box
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
		vboxAddStyleItem parent box ctxt activeItemSvg textStyleDialogInfo latestConfig styleGet styleSet
		
	containerAdd vbtnBox copyBtn
	editBtn <- prepareButton stockEdit
	editBtn `on` buttonActivated $ do
		conf <- readIORef latestConfig
		showTextStyleDialog parent textStyleDialogInfo (confTextStyleGetter conf) styleUpdatedCallback
 	containerAdd vbtnBox editBtn
	deleteBtn <- prepareButton stockDelete
	deleteBtn `on` buttonActivated $ do
		userConfirms <- userConfirmDelete parent
		when userConfirms $ updateConfig latestConfig $ removeTextStyle parent confTextStyleGetter box
		
	containerAdd vbtnBox deleteBtn
	boxPackStart hbox vbtnBox PackNatural 0

	boxPackStart box hbox PackNatural 0
	widgetShowAll hbox

changeSelectedConfig :: (Conf->TextStyle) -> Box -> Conf -> IO Conf
changeSelectedConfig confTextStyleGetter box conf = do
	let clickedStyleId = styleId $ confTextStyleGetter conf
	let newConf = setSetting conf selectedTextStyleId clickedStyleId
	widgetQueueDraw box
	return newConf

removeTextStyle :: Dialog -> (Conf->TextStyle) -> Box -> Conf -> IO Conf
removeTextStyle parent confTextStyleGetter box conf = do
	let styleIdToRemove = styleId $ confTextStyleGetter conf
	let cStyles = getSetting' conf textStyles
	if styleIdToRemove == getSetting' conf selectedTextStyleId
		then do
			displayError parent "Cannot delete the selected text style"
			return conf
		else do
			let styleIdx = fromJust $ findIndex ((==styleIdToRemove) . styleId) cStyles
			let newConf = setSetting conf textStyles $ filter ((/=styleIdToRemove) . styleId) cStyles
			boxWidgets <- containerGetChildren box
			containerRemove box $ boxWidgets !! styleIdx
			return newConf

displayError :: WindowClass a => a -> String -> IO ()
displayError parent msg = do
	dialog <- messageDialogNew (Just $ toWindow parent) [DialogModal] MessageError ButtonsOk msg
	dialogRun dialog
	widgetDestroy dialog

userConfirmDelete :: WindowClass a => a -> IO Bool
userConfirmDelete parent = do
	dialog <- messageDialogNew (Just $ toWindow parent) [DialogModal] MessageWarning ButtonsYesNo "Sure to delete the text style?"
	resp <- dialogRun dialog
	widgetDestroy dialog
	return $ resp == ResponseYes

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
		renderText text cTextStyle

	builderGetObject builder castToColorButton "fillColor" >>= bindModel textStyleModel textFillL
	builderGetObject builder castToColorButton "strokeColor" >>= bindModel textStyleModel textStrokeL

	borderScale <- builderGetObject builder castToScale "borderScale"
	borderAdjustment <- adjustmentNew (strokeHeightRatio textStyle *100) 0 11 1 1 1
	rangeSetAdjustment borderScale borderAdjustment
	bindModel textStyleModel strokeHeightRatioL borderAdjustment

	fontButton <- builderGetObject builder castToFontButton "fontButton"
	bindModel textStyleModel fontNameL fontButton

	-- TODO we're changing the font when any setting changes.
	-- no need to recompute the font when the colors are changed
	-- for instance.
	addModelObserver textStyleModel $ \cTextStyle -> do
		updateFontFromTextStyle ctxt cTextStyle
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

contextSetFontSize :: PangoContext -> Double -> IO ()
contextSetFontSize ctxt fontSize = do
	font <- contextGetFontDescription ctxt
	fontDescriptionSetSize font fontSize
	contextSetFontDescription ctxt font

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

updateFontFromTextStyle :: PangoContext -> TextStyle -> IO ()
updateFontFromTextStyle ctxt textStyle = do
	font <- case fontName textStyle of
		Nothing -> contextGetFontDescription ctxt
		Just name -> liftIO $ fontDescriptionFromString name
	--liftIO $ layoutSetFontDescription text (Just fontDesc)
	contextSetFontDescription ctxt font

updateConfig :: IORef Conf -> (Conf -> IO Conf) -> IO ()
updateConfig latestConfig newConfigMaker = do
	conf <- readIORef latestConfig
	conf' <- newConfigMaker conf
	saveSettings conf'
	writeIORef latestConfig conf'

renderText :: PangoLayout -> TextStyle -> Render ()
renderText text textStyle = do
	layoutPath text
	setSourceRGBA `applyColor` textFill textStyle
	fillPreserve
	setSourceRGBA `applyColor` textStroke textStyle
	(Rectangle _ _ _ rHeight) <- liftM snd $ liftIO (layoutGetPixelExtents text)
	setLineWidth $ fromIntegral rHeight * strokeHeightRatio textStyle
	strokePreserve
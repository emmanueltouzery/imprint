{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.Rendering.Cairo hiding (width, height, x)
import Graphics.UI.Gtk hiding (styleSet)
import Graphics.HsExif (parseFileExif, getDateTimeOriginal)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Control.Monad (liftM, when)
import Data.Maybe (fromJust, isJust)
import Data.AppSettings (GetSetting(..), getSetting', Conf, setSetting)
import Data.IORef
import Control.Lens hiding (set)
import Data.List

import Helpers
import Settings
import GtkMvvm

minFontSize :: Int
minFontSize = 5

main :: IO ()
main = do
	initGUI

	-- TODO this must go in a try
	-- This is the settings dialog.
	-- Therefore it's a special situation because
	-- the settings can change anytime.
	-- in the rest of the app however they'll be static.
	(settings, GetSetting getSetting) <- Settings.readSettings

	-- ############ TODO I think i don't need to hold the config in an IORef
	-- now. I want realtime edit when previewing changes in the OK/Cancel dialog.
	-- But the rest of the time I'll in fact reload. So I think I can limit the
	-- IORef to the scope of the OK/Cancel preview dialog.
	latestConfig <- newIORef settings

	builder <- builderNew
	builderAddFromFile builder "imprint.ui"

	let filename = "DSC04293.JPG"

	exifInfo <- parseFileExif filename
	let exifData = case exifInfo of
		Left errorStr -> error errorStr
		Right exif -> exif
	let picDateTime = case getDateTimeOriginal exifData of
		Nothing -> error "No date info in EXIF"
		Just v -> v
	print picDateTime
	let formattedDate = formatTime defaultTimeLocale "%x" picDateTime

	img <- pixbufNewFromFile filename
	width <- pixbufGetWidth img
	height <- pixbufGetHeight img
	sur <- createImageSurface FormatRGB24 width height
	ctxt <- cairoCreateContext Nothing
	text <- layoutEmpty ctxt
	text `layoutSetText` formattedDate
	updateFontFromTextStyle ctxt $ getSetting selectedTextStyle

	let textSizePoints = fromIntegral width * getSetting textSizeFromWidth
	contextSetFontSize ctxt textSizePoints

	renderWith sur $ do
		setSourcePixbuf img 0 0
		paint
		let marginX = floor $ fromIntegral width * getSetting marginXFromWidth
		let marginY = floor $ fromIntegral width * getSetting marginYFromWidth
		(Rectangle _ _ rWidth rHeight) <- liftM snd $ liftIO (layoutGetPixelExtents text)
		moveTo (fromIntegral $ width - rWidth - marginX)
			(fromIntegral $ height - rHeight - marginY)
		renderText text (getSetting selectedTextStyle)

	pbuf <- pixbufNewFromSurface sur 0 0 width height
	pixbufSave pbuf "newout.jpg" "jpeg" [("quality", "95")]
	Settings.saveSettings settings

	showTextStyleListDialog builder latestConfig
	mainGUI

showTextStyleListDialog :: Builder -> IORef Conf -> IO ()
showTextStyleListDialog builder latestConfig = do
	dialog <- builderGetObject builder castToWindow "window1"
	stylesVbox <- builderGetObject builder castToBox "stylesVbox"
	containerForeach stylesVbox (\w -> containerRemove stylesVbox w)
	conf <- readIORef latestConfig
	textStyleDialogInfo <- prepareTextStyleDialog builder (getSetting' conf selectedTextStyle)
	ctxt <- cairoCreateContext Nothing

	let styles = getSetting' conf textStyles
	let styleGettersSetters = fmap (getStyleGetterSetter stylesVbox latestConfig) [0..length styles-1]

	mapM_ (uncurry $ vboxAddStyleItem dialog stylesVbox ctxt textStyleDialogInfo latestConfig) styleGettersSetters
	windowSetDefaultSize dialog 600 500
	widgetShowAll dialog

getStyleGetterSetter :: Box -> IORef Conf -> Int -> (Conf->TextStyle, TextStyle -> IO ())
getStyleGetterSetter stylesVbox latestConfig idx = (styleGetter, updateStyle latestConfig stylesVbox idx)
	where styleGetter = \cnf -> (getSetting' cnf textStyles) !! idx

updateStyle :: IORef Conf -> Box -> Int -> TextStyle -> IO ()
updateStyle latestConfig stylesVbox styleIdx newStyle = do
	c <- readIORef latestConfig
	let styles = getSetting' c textStyles
	let newConf = setSetting c textStyles $ styles & ix styleIdx .~ newStyle
	Settings.saveSettings newConf
	writeIORef latestConfig newConf
	widgetQueueDraw stylesVbox

prepareTextStyleDrawingArea :: PangoContext -> PangoLayout -> DrawingArea -> IO ()
prepareTextStyleDrawingArea ctxt text drawingArea = do
	drawingArea `on` configureEvent $ do
		-- widget resize
		liftIO $ setFontSizeForWidget ctxt text drawingArea
		return True
	return ()

-- Maybe could use Gtk signals for the styleUpdatedCallback,
-- but don't know how/whether it's possible.
vboxAddStyleItem :: Window -> Box -> PangoContext -> TextStyleDialogInfo -> IORef Conf -> (Conf->TextStyle)
		-> (TextStyle -> IO ()) -> IO ()
vboxAddStyleItem parent box ctxt textStyleDialogInfo latestConfig confTextStyleGetter styleUpdatedCallback = do
	text <- layoutEmpty ctxt
	text `layoutSetText` "2014-04-01"

	drawingArea <- drawingAreaNew
	widgetSetSizeRequest drawingArea 500 100

	prepareTextStyleDrawingArea ctxt text drawingArea
	drawingArea `on` draw $ do
		conf <- liftIO $ readIORef latestConfig
		let cTextStyle = confTextStyleGetter conf
		liftIO $ do
			updateFontFromTextStyle ctxt cTextStyle
			setFontSizeForWidget ctxt text drawingArea
		renderText text cTextStyle

	-- TODO move to GTK/glade widget templates
	hbox <- hBoxNew False 0
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
		let (styleGet, styleSet) = getStyleGetterSetter box latestConfig $ length styles
		vboxAddStyleItem parent box ctxt textStyleDialogInfo latestConfig styleGet styleSet
		
	containerAdd vbtnBox copyBtn
	editBtn <- prepareButton stockEdit
	editBtn `on` buttonActivated $ do
		conf <- readIORef latestConfig
		showTextStyleDialog parent textStyleDialogInfo (confTextStyleGetter conf) styleUpdatedCallback
 	containerAdd vbtnBox editBtn
	deleteBtn <- prepareButton stockDelete
	deleteBtn `on` buttonActivated $ do
		userConfirms <- userConfirmDelete parent
		when userConfirms $ do
			conf <- readIORef latestConfig
			let curStyleId = styleId $ confTextStyleGetter conf
			let cStyles = getSetting' conf textStyles
			let styleIdx = fromJust $ findIndex ((==curStyleId) . styleId) cStyles
			let newConf = setSetting conf textStyles $ filter ((/=curStyleId) . styleId) cStyles
			Settings.saveSettings newConf
			writeIORef latestConfig newConf
			boxWidgets <- containerGetChildren box
			containerRemove box $ boxWidgets !! styleIdx
		
	containerAdd vbtnBox deleteBtn
	boxPackStart hbox vbtnBox PackNatural 0

	boxPackStart box hbox PackNatural 0
	widgetShowAll hbox

userConfirmDelete :: Window -> IO Bool
userConfirmDelete parent = do
	dialog <- messageDialogNew (Just parent) [DialogModal] MessageWarning ButtonsYesNo "Sure to delete the text style?"
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

showTextStyleDialog :: Window -> TextStyleDialogInfo -> TextStyle -> (TextStyle -> IO ()) -> IO ()
showTextStyleDialog parent (TextStyleDialogInfo curTextStyle textStyleBtnOk dialog okSigRef) textStyle updateCallback = do
	modifyModel curTextStyle $ const textStyle
	okSig <- readIORef okSigRef
	when (isJust okSig) $ signalDisconnect $ fromJust okSig
	newOkSig <- textStyleBtnOk `on` buttonActivated $ do
		readModel curTextStyle >>= updateCallback
		widgetHide dialog
	writeIORef okSigRef (Just newOkSig)
	
	windowSetDefaultSize dialog 450 400
	set dialog [windowTransientFor := parent]
	dialogRun dialog
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

updateConfig :: IORef Conf -> (Conf -> Conf) -> IO ()
updateConfig latestConfig newConfigMaker = do
	conf <- readIORef latestConfig
	let conf' = newConfigMaker conf
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

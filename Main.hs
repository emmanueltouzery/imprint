{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.Rendering.Cairo hiding (width, height)
import Graphics.UI.Gtk
import Graphics.HsExif (parseFileExif, getDateTimeOriginal)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Control.Monad (liftM, when)
import Data.Maybe (fromJust, isJust)
import Data.AppSettings (GetSetting(..), getSetting', Conf)
import Data.IORef
import Control.Lens hiding (Setting, setting, set)

import Helpers
import Settings

minFontSize :: Int
minFontSize = 5

pixelToPoints :: Int -> Double
pixelToPoints pixels = (fromIntegral pixels :: Double) * 72 / 96

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

	textStyleDialogInfo <- prepareTextStyleDialog builder (getSetting selectedTextStyle)

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

	showTextStyleListDialog builder latestConfig textStyleDialogInfo
	mainGUI

showTextStyleListDialog :: Builder -> IORef Conf -> TextStyleDialogInfo -> IO ()
showTextStyleListDialog builder latestConfig textStyleDialogInfo = do
	dialog <- builderGetObject builder castToWindow "window1"
	stylesVbox <- builderGetObject builder castToBox "stylesVbox"
	containerForeach stylesVbox (\w -> containerRemove stylesVbox w)
	conf <- readIORef latestConfig
	let styles = getSetting' conf textStyles
	ctxt <- cairoCreateContext Nothing
	
	let editCallbacks = fmap (updateStyle latestConfig stylesVbox) [0..]
	mapM_ (uncurry $ vboxAddStyleItem dialog stylesVbox ctxt textStyleDialogInfo) $ zip styles editCallbacks
	windowSetDefaultSize dialog 600 500
	widgetShowAll dialog

updateStyle :: IORef Conf -> Box -> Int -> TextStyle -> IO ()
updateStyle latestConfig stylesVbox styleIdx newStyle = putStrLn "update style!"

prepareTextStyleDrawingArea :: PangoContext -> PangoLayout -> DrawingArea -> IO ()
prepareTextStyleDrawingArea ctxt text drawingArea = do
	drawingArea `on` configureEvent $ do
		-- widget resize
		liftIO $ setFontSizeForWidget ctxt text drawingArea
		return True
	return ()

-- Maybe could use Gtk signals for the styleUpdatedCallback,
-- but don't know how/whether it's possible.
vboxAddStyleItem :: Window -> Box -> PangoContext -> TextStyleDialogInfo -> TextStyle -> (TextStyle -> IO ()) -> IO ()
vboxAddStyleItem parent box ctxt textStyleDialogInfo textStyle styleUpdatedCallback = do
	text <- layoutEmpty ctxt
	text `layoutSetText` "2014-04-01"

	drawingArea <- drawingAreaNew
	widgetSetSizeRequest drawingArea 500 100

	prepareTextStyleDrawingArea ctxt text drawingArea
	drawingArea `on` draw $ renderText text textStyle

	-- TODO move to GTK/glade widget templates
	hbox <- hBoxNew False 0
	boxPackStart hbox drawingArea PackNatural 0
	vbtnBox <- vButtonBoxNew
	prepareButton stockCopy >>= containerAdd vbtnBox
	editBtn <- prepareButton stockEdit
	editBtn `on` buttonActivated $ do
		showTextStyleDialog parent textStyleDialogInfo textStyle styleUpdatedCallback
 	containerAdd vbtnBox editBtn
	prepareButton stockDelete >>= containerAdd vbtnBox
	boxPackStart hbox vbtnBox PackNatural 0

	boxPackStart box hbox PackNatural 0
	widgetShowAll hbox

prepareButton :: StockId -> IO Button
prepareButton stockId = do
	btn <- buttonNew
	img <- imageNewFromStock stockId IconSizeSmallToolbar
	buttonSetImage btn img
	buttonSetRelief btn ReliefNone
	return btn

data TextStyleDialogInfo = TextStyleDialogInfo (IORef TextStyle) Button Dialog (IORef (Maybe (ConnectId Button)))

prepareTextStyleDialog :: Builder -> TextStyle -> IO TextStyleDialogInfo
prepareTextStyleDialog builder textStyle = do
	ctxt <- cairoCreateContext Nothing
	text <- layoutEmpty ctxt
	text `layoutSetText` "2014-04-01"

	curTextStyle <- newIORef textStyle

	dialog <- builderGetObject builder castToDialog "settings_dialog"
	textPreview <- builderGetObject builder castToDrawingArea "textPreview"

	prepareTextStyleDrawingArea ctxt text textPreview
	textPreview `on` draw $ do
		cTextStyle <- liftIO $ readIORef curTextStyle
		renderText text cTextStyle

	tieColor dialog builder "fillColor" curTextStyle textFillL
	tieColor dialog builder "strokeColor" curTextStyle textStrokeL
	borderScale <- builderGetObject builder castToScale "borderScale"
	borderAdjustment <- adjustmentNew (strokeHeightRatio textStyle *100) 0 11 1 1 1
	rangeSetAdjustment borderScale borderAdjustment
	dialog `on` mapEvent $ liftIO $ do
		cTextStyle <- readIORef curTextStyle
		adjustmentSetValue borderAdjustment (strokeHeightRatio cTextStyle *100)
		return False
	onValueChanged borderAdjustment $ do
		newRatio <- liftM (/100) $ adjustmentGetValue borderAdjustment
		modifyIORef curTextStyle (strokeHeightRatioL .~ newRatio)
		widgetQueueDraw textPreview

	fontButton <- builderGetObject builder castToFontButton "fontButton"
	dialog `on` mapEvent $ liftIO $ do
		cTextStyle <- readIORef curTextStyle
		fontButtonSetFontName fontButton $ if (isJust $ fontName cTextStyle)
			then fromJust $ fontName cTextStyle
			else ""
		return False
		
	onFontSet fontButton $ do
		selectedFontName <- fontButtonGetFontName fontButton
		modifyIORef curTextStyle (fontNameL .~ Just selectedFontName)
		readIORef curTextStyle >>= updateFontFromTextStyle ctxt
		setFontSizeForWidget ctxt text textPreview

	textStyleBtnCancel <- builderGetObject builder castToButton "textStyleBtnCancel"
	textStyleBtnCancel `on` buttonActivated $ widgetHide dialog

	textStyleBtnOk <- builderGetObject builder castToButton "textStyleBtnOk"

	okSignalRef <- newIORef Nothing

	return $ TextStyleDialogInfo curTextStyle textStyleBtnOk dialog okSignalRef

showTextStyleDialog :: Window -> TextStyleDialogInfo -> TextStyle -> (TextStyle -> IO ()) -> IO ()
showTextStyleDialog parent (TextStyleDialogInfo curTextStyle textStyleBtnOk dialog okSigRef) textStyle updateCallback = do
	writeIORef curTextStyle textStyle
	okSig <- readIORef okSigRef
	when (isJust okSig) $ signalDisconnect $ fromJust okSig
	newOkSig <- textStyleBtnOk `on` buttonActivated $ do
		readIORef curTextStyle >>= updateCallback
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

tieColor :: Dialog -> Builder -> String -> IORef TextStyle -> (Lens' TextStyle ColorRgba) -> IO ()
tieColor dialog builder buttonName curTextStyle colorL = do
	colorBtn <- builderGetObject builder castToColorButton buttonName
	dialog `on` mapEvent $ liftIO $ do
		textStyle <- readIORef curTextStyle
		buttonSetColor colorBtn $ textStyle ^. colorL
		return False
	onColorSet colorBtn $ colorChanged curTextStyle colorBtn colorL
	return ()

colorChanged :: IORef TextStyle -> ColorButton -> (Lens' TextStyle ColorRgba) -> IO ()
colorChanged curTextStyle btn colorL = do
	putStrLn "color changed"
	gtkColor <- colorButtonGetColor btn
	alpha <- colorButtonGetAlpha btn
	modifyIORef curTextStyle (colorL .~ readGtkColorAlpha gtkColor alpha)

renderText :: PangoLayout -> TextStyle -> Render ()
renderText text textStyle = do
	layoutPath text
	setSourceRGBA `applyColor` textFill textStyle
	fillPreserve
	setSourceRGBA `applyColor` textStroke textStyle
	(Rectangle _ _ _ rHeight) <- liftM snd $ liftIO (layoutGetPixelExtents text)
	setLineWidth $ fromIntegral rHeight * strokeHeightRatio textStyle
	strokePreserve

{-# LANGUAGE RankNTypes #-}

module Main where

import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Cairo
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.HsExif (parseFileExif, getDateTimeOriginal)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Control.Monad (liftM, when)
import Data.Maybe (fromJust, isJust)
import Data.AppSettings (GetSetting(..), getSetting', Conf, Setting, setSetting)
import Data.IORef
import Control.Lens hiding (Setting, setting)

import Helpers
import Settings

minFontSize :: Int
minFontSize = 5

pixelToPoints :: Int -> Double
pixelToPoints pixels = (fromIntegral pixels :: Double) * 72 / 96

main = do
	initGUI

	-- TODO this must go in a try
	-- This is the settings dialog.
	-- Therefore it's a special situation because
	-- the settings can change anytime.
	-- in the rest of the app however they'll be static.
	(settings, GetSetting getSetting) <- Settings.readSettings

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
	updateFontFromSettings ctxt width latestConfig

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
		renderText text (getSetting selectedTextStyle) width

	pbuf <- pixbufNewFromSurface sur 0 0 width height
	pixbufSave pbuf "newout.jpg" "jpeg" [("quality", "95")]
	Settings.saveSettings settings

	showTextStyleListDialog builder latestConfig
	showTextStyleDialog builder (GetSetting getSetting) latestConfig
	mainGUI

showTextStyleListDialog :: Builder -> IORef Conf -> IO ()
showTextStyleListDialog builder latestConfig = do
	dialog <- builderGetObject builder castToWindow "window1"
	stylesVbox <- builderGetObject builder castToBox "stylesVbox"
	containerForeach stylesVbox (\w -> containerRemove stylesVbox w)
	conf <- readIORef latestConfig
	let styles = getSetting' conf textStyles
	ctxt <- cairoCreateContext Nothing
	mapM_ (vboxAddStyleDrawable stylesVbox ctxt) styles
	windowSetDefaultSize dialog 600 400
	widgetShowAll dialog

prepareTextStyleDrawingArea :: IORef Conf -> PangoContext -> PangoLayout -> DrawingArea -> IO ()
prepareTextStyleDrawingArea latestConfig ctxt text drawingArea = do
	drawingArea `on` configureEvent $ do
		-- widget resize
		liftIO $ setFontSizeForWidget ctxt text drawingArea
		return True

	drawingArea `on` draw $ updateTextPreview drawingArea text latestConfig
	return ()

vboxAddStyleDrawable :: Box -> PangoContext -> TextStyle -> IO ()
vboxAddStyleDrawable box ctxt textStyle = do
	-- ### NEXT TWO LINES JUST FOR TESTING ###
	-- WITH THIS CODE I'LL ALWAYS DISPLAY THE CURRENT STYLE,
	-- FOR ALL THE ITEMS IN THE LIST!
	(settings, GetSetting getSetting) <- Settings.readSettings
	latestConfig <- newIORef settings

	text <- layoutEmpty ctxt
	text `layoutSetText` "2014-04-01"

	drawingArea <- drawingAreaNew
	widgetSetSizeRequest drawingArea (-1) 100

	prepareTextStyleDrawingArea latestConfig ctxt text drawingArea
	boxPackStart box drawingArea PackNatural 0
	widgetShowAll drawingArea

showTextStyleDialog :: Builder -> GetSetting -> IORef Conf -> IO ()
showTextStyleDialog builder (GetSetting getSetting) latestConfig = do

	ctxt <- cairoCreateContext Nothing
	text <- layoutEmpty ctxt
	text `layoutSetText` "2014-04-01"

	dialog <- builderGetObject builder castToDialog "settings_dialog"
	textPreview <- builderGetObject builder castToDrawingArea "textPreview"

	prepareTextStyleDrawingArea latestConfig ctxt text textPreview

	let initialTextStyle = getSetting selectedTextStyle

	tieColor builder "fillColor" latestConfig textFillL
	tieColor builder "strokeColor" latestConfig textStrokeL
	borderScale <- builderGetObject builder castToScale "borderScale"
	borderAdjustment <- adjustmentNew (strokeHeightRatio initialTextStyle *100) 0 11 1 1 1
	rangeSetAdjustment borderScale borderAdjustment
	onValueChanged borderAdjustment $ do
		newRatio <- liftM (/100) $ adjustmentGetValue borderAdjustment
		updateConfigSetting latestConfig selectedTextStyle (strokeHeightRatioL .~ newRatio)
		widgetQueueDraw textPreview

	fontButton <- builderGetObject builder castToFontButton "fontButton"
	when (isJust $ fontName initialTextStyle) $ do
		fontButtonSetFontName fontButton $ fromJust $ fontName initialTextStyle
		return ()
		
	onFontSet fontButton $ do
		selectedFontName <- fontButtonGetFontName fontButton
		updateConfigSetting latestConfig selectedTextStyle (fontNameL .~ Just selectedFontName)
		w <- widgetGetAllocatedWidth textPreview
		updateFontFromSettings ctxt w latestConfig
		setFontSizeForWidget ctxt text textPreview

	widgetShowAll dialog

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
	(Rectangle _ _ rWidth rHeight) <- liftM snd $ layoutGetPixelExtents text
	if rWidth < maxWidth && rHeight < maxHeight
		then setFontSizeForBoundingBox ctxt text (fontSize+1) maxWidth maxHeight
		else contextSetFontSize ctxt $ fromIntegral $ fontSize-1

updateFontFromSettings :: PangoContext -> Int -> IORef Conf -> IO ()
updateFontFromSettings ctxt width latestConfig = do
	conf <- readIORef latestConfig
	font <- case fontName $ getSetting' conf selectedTextStyle of
		Nothing -> contextGetFontDescription ctxt
		Just name -> liftIO $ fontDescriptionFromString name
	--liftIO $ layoutSetFontDescription text (Just fontDesc)
	contextSetFontDescription ctxt font

updateConfigSetting :: (Read a, Show a) => IORef Conf -> Setting a -> (a->a) -> IO ()
updateConfigSetting latestConfig setting settingUpdater = updateConfig latestConfig $
	\conf -> setSetting conf setting $ settingUpdater $ getSetting' conf setting

updateConfig :: IORef Conf -> (Conf -> Conf) -> IO ()
updateConfig latestConfig newConfigMaker = do
	conf <- readIORef latestConfig
	let conf' = newConfigMaker conf
	saveSettings conf'
	writeIORef latestConfig conf'

tieColor :: Builder -> String -> IORef Conf -> (Lens' TextStyle ColorRgba) -> IO ()
tieColor builder buttonName latestConfig colorL = do
	colorBtn <- builderGetObject builder castToColorButton buttonName
	conf <- readIORef latestConfig
	let textStyle = getSetting' conf selectedTextStyle
	buttonSetColor colorBtn $ textStyle ^. colorL
	onColorSet colorBtn $ colorChanged latestConfig colorBtn colorL
	return ()

colorChanged :: IORef Conf -> ColorButton -> (Lens' TextStyle ColorRgba) -> IO ()
colorChanged latestConfig btn colorL = do
	putStrLn "color changed"
	gtkColor <- colorButtonGetColor btn
	alpha <- colorButtonGetAlpha btn
	updateConfigSetting latestConfig selectedTextStyle (colorL .~ readGtkColorAlpha gtkColor alpha)

renderText :: PangoLayout -> TextStyle -> Int -> Render ()
renderText text textStyle width = do
	layoutPath text
	setSourceRGBA `applyColor` textFill textStyle
	fillPreserve
	setSourceRGBA `applyColor` textStroke textStyle
	(Rectangle _ _ _ rHeight) <- liftM snd $ liftIO (layoutGetPixelExtents text)
	setLineWidth $ fromIntegral rHeight * strokeHeightRatio textStyle
	strokePreserve

updateTextPreview :: WidgetClass widget => widget -> PangoLayout -> IORef Conf -> Render ()
updateTextPreview widget text latestConfig = do
	conf <- liftIO $ readIORef latestConfig
  	width  <- liftIO $ widgetGetAllocatedWidth widget
	renderText text (getSetting' conf selectedTextStyle) width

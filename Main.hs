module Main where

import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Cairo
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.HsExif (parseFileExif, getDateTimeOriginal)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import Data.AppSettings (GetSetting(..), getSetting', Conf, Setting, setSetting)
import Data.IORef

import Helpers
import Settings

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
	builderAddFromFile builder "settings.ui"

	dialog <- builderGetObject builder castToDialog "settings_dialog"

	textPreview <- builderGetObject builder castToDrawingArea "textPreview"

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
	font <- contextGetFontDescription ctxt
	let textSizePoints = floor $ fromIntegral width * getSetting textSizeFromWidth
	fontDescriptionSetSize font (pixelToPoints $ textSizePoints)
	contextSetFontDescription ctxt font

	renderWith sur $ do
		setSourcePixbuf img 0 0
		paint
		let marginX = floor $ fromIntegral width * getSetting marginXFromWidth
		let marginY = floor $ fromIntegral width * getSetting marginYFromWidth
		(Rectangle _ _ rWidth rHeight) <- liftM snd $ liftIO (layoutGetPixelExtents text)
		moveTo (fromIntegral $ width - rWidth - marginX)
			(fromIntegral $ height - rHeight - marginY)
		renderText text (GetSetting getSetting) width

	pbuf <- pixbufNewFromSurface sur 0 0 width height
	pixbufSave pbuf "newout.jpg" "jpeg" [("quality", "95")]
	Settings.saveSettings settings

	textPreview `on` draw $ updateTextPreview textPreview text latestConfig

	tieColor builder "fillColor" latestConfig textFill
	tieColor builder "strokeColor" latestConfig textStroke
	borderScale <- builderGetObject builder castToScale "borderScale"
	borderAdjustment <- adjustmentNew (getSetting strokeHeightRatio*100) 0 11 1 1 1
	rangeSetAdjustment borderScale borderAdjustment
	onValueChanged borderAdjustment $ do
		newRatio <- liftM (/100) $ adjustmentGetValue borderAdjustment
		updateConfig latestConfig $ \conf -> setSetting conf strokeHeightRatio newRatio
		widgetQueueDraw textPreview

	widgetShowAll dialog
	mainGUI

updateConfig :: IORef Conf -> (Conf -> Conf) -> IO ()
updateConfig latestConfig newConfigMaker = do
	conf <- readIORef latestConfig
	let conf' = newConfigMaker conf
	saveSettings conf'
	writeIORef latestConfig conf'

tieColor :: Builder -> String -> IORef Conf -> Setting (Double, Double, Double, Double) -> IO ()
tieColor builder buttonName latestConfig colorSetting = do
	colorBtn <- builderGetObject builder castToColorButton buttonName
	conf <- readIORef latestConfig
	buttonSetColor colorBtn $ getSetting' conf colorSetting
	onColorSet colorBtn $ colorChanged latestConfig colorBtn colorSetting
	return ()

colorChanged :: IORef Conf -> ColorButton -> Setting (Double, Double, Double, Double) -> IO ()
colorChanged latestConfig btn setting = do
	putStrLn "color changed"
	gtkColor <- colorButtonGetColor btn
	alpha <- colorButtonGetAlpha btn
	updateConfig latestConfig $ \conf -> setSetting conf setting $ readGtkColorAlpha gtkColor alpha

renderText :: PangoLayout -> GetSetting -> Int -> Render ()
renderText text (GetSetting getSetting) width = do
	layoutPath text

	setSourceRGBA `applyColor` getSetting textFill
	fillPreserve
	setSourceRGBA `applyColor` getSetting textStroke
	(Rectangle _ _ _ rHeight) <- liftM snd $ liftIO (layoutGetPixelExtents text)
	setLineWidth $ fromIntegral rHeight * getSetting strokeHeightRatio
	strokePreserve

updateTextPreview :: WidgetClass widget => widget -> PangoLayout -> IORef Conf -> Render ()
updateTextPreview widget text latestConfig = do
	conf <- liftIO $ readIORef latestConfig
  	width  <- liftIO $ widgetGetAllocatedWidth widget
	renderText text (GetSetting $ getSetting' conf) width

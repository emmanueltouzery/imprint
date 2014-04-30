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
import Data.AppSettings (GetSetting(..), Setting, setSetting)

import Helpers
import Settings

pixelToPoints :: Int -> Double
pixelToPoints pixels = (fromIntegral pixels :: Double) * 72 / 96

main = do
	initGUI

	-- TODO this must go in a try
	(settings, GetSetting getSetting) <- Settings.readSettings

	builder <- builderNew
	builderAddFromFile builder "settings.ui"

	dialog <- builderGetObject builder castToDialog "settings_dialog"

	textPreview <- builderGetObject builder castToDrawingArea "textPreview"
	fillColor <- builderGetObject builder castToColorButton "fillColor"
	buttonSetColor fillColor $ getSetting textFill
	onColorSet fillColor $ colorChanged fillColor textFill

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

	textPreview `on` draw $ updateTextPreview textPreview text $ GetSetting getSetting

	widgetShowAll dialog
	mainGUI

colorChanged :: ColorButton -> Setting (Double, Double, Double, Double) -> IO ()
colorChanged btn setting = do
	putStrLn "color changed"
	gtkColor <- colorButtonGetColor btn
	alpha <- colorButtonGetAlpha btn
	conf <- liftM fst Settings.readSettings
	let conf' = setSetting conf setting $ readGtkColorAlpha gtkColor alpha
	saveSettings conf'

renderText :: PangoLayout -> GetSetting -> Int -> Render ()
renderText text (GetSetting getSetting) width = do
	layoutPath text

	liftIO $ putStrLn "before drawing text"
	setSourceRGBA `applyColor` getSetting textFill
	fillPreserve
	setSourceRGBA `applyColor` getSetting textStroke
	setLineWidth $ fromIntegral width * getSetting strokeWidthFromWidth
	strokePreserve
	liftIO $ putStrLn "after drawing text"

updateTextPreview :: WidgetClass widget => widget -> PangoLayout -> GetSetting -> Render ()
updateTextPreview widget text getSetting' = do
  	width  <- liftIO $ widgetGetAllocatedWidth widget
	renderText text getSetting' width

module Main where

import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Cairo
import Graphics.HsExif (parseFileExif, getDateTimeOriginal)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Control.Monad (liftM)
import Data.Maybe (fromJust)

import Helpers
import qualified Settings

pixelToPoints :: Int -> Double
pixelToPoints pixels = (fromIntegral pixels :: Double) * 72 / 96

main = do
	initGUI

	settings <- Settings.readSettings

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
	let textSizePoints = floor $ fromIntegral width * Settings.textSizeFromWidth settings
	fontDescriptionSetSize font (pixelToPoints $ textSizePoints)
	contextSetFontDescription ctxt font

	let marginX = floor $ fromIntegral width * Settings.marginXFromWidth settings
	let marginY = floor $ fromIntegral width * Settings.marginYFromWidth settings

	renderWith sur $ do

		setSourcePixbuf img 0 0
		paint

		inkExtents <- liftM snd $ liftIO (layoutGetPixelExtents text)
		moveTo (fromIntegral $ width - rectWidth inkExtents - marginX)
			(fromIntegral $ height - rectHeight inkExtents - marginY)
		layoutPath text

		liftIO $ putStrLn "before drawing text"
		setSourceRGBA `applyColor` Settings.textFill settings
		fillPreserve
		setSourceRGBA `applyColor` Settings.textStroke settings
		setLineWidth $ fromIntegral width * Settings.textStrokeWidthFromWidth settings
		strokePreserve
		liftIO $ putStrLn "after drawing text"
	pbuf <- pixbufNewFromSurface sur 0 0 width height
	pixbufSave pbuf "newout.jpg" "jpeg" [("quality", "95")]
	--Settings.saveSettings Settings.defaultSettings

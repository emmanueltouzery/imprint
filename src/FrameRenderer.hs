{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module FrameRenderer (renderFrame, renderText, parseFormat, FormatElement(..)) where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo hiding (width, height, x)
import Control.Monad (liftM)
import Graphics.HsExif
import Text.ParserCombinators.Parsec

import Settings
import Helpers

data FormatElement = StringContents String
	| DateFormat String
	| ExifContents ExifTag
	| Filename
	deriving (Show, Eq)

parseFormat :: String -> Either String [FormatElement]
parseFormat input = case parse parseFormatParsec "" input of
	Left parseError -> Left $ show parseError
	Right result -> Right result

parseFormatParsec :: GenParser Char st [FormatElement]
parseFormatParsec = many1 parseFormatElement

parseFormatElement :: GenParser Char st FormatElement
parseFormatElement = (try parseDate) <|> (try parseEscapedPercent) <|> (try parseFormatItem) <|> parseString

parseDate :: GenParser Char st FormatElement
parseDate = do
	string "%date{"
	dateFormat <- manyTill anyChar $ try $ char '}'
	return $ DateFormat dateFormat

parseEscapedPercent :: GenParser Char st FormatElement
parseEscapedPercent = do
	string "%%"
	return $ StringContents "%"

parseFormatItem :: GenParser Char st FormatElement
parseFormatItem = do
	string "%"
	format <- many1 $ noneOf " \t\r\n"
	return $ case format of
		"file" -> Filename
		"expo" -> ExifContents exposureTime
		"aper" -> ExifContents fnumber
		"iso" -> ExifContents isoSpeedRatings
		"expo_bias" -> ExifContents exposureBiasValue
		"make" -> ExifContents make
		"model" -> ExifContents model
		"soft" -> ExifContents software
		"copy" -> ExifContents copyright
		"focal35" -> ExifContents focalLengthIn35mmFilm
		_ -> error $ "Unknown format type: " ++ format
	
parseString :: GenParser Char st FormatElement
parseString = do
	-- don't want to eat the %!
	contents <- manyTill anyChar $ try $ char '%'
	return $ StringContents contents

-- TODO not possible to get the current pango context myself?
renderFrame :: Int -> Int -> PangoLayout -> PangoContext -> [(DisplayItem, TextStyle)] -> Render ()
renderFrame width height text ctxt = mapM_ $ uncurry $ renderDisplayItem width height text ctxt

renderDisplayItem :: Int -> Int -> PangoLayout -> PangoContext -> DisplayItem -> TextStyle -> Render ()
renderDisplayItem width height text ctxt displayItem textStyle = do
	let marginX = floor $ fromIntegral width * marginXFromWidth displayItem
	let marginY = floor $ fromIntegral width * marginYFromWidth displayItem
	let textSizePoints = fromIntegral width * textSizeFromWidth displayItem
	-- renderText will also set the font, but I must do it
	-- before already to get the right font metrics...
	liftIO $ updateFontFromTextStyle ctxt text textStyle textSizePoints
	(Rectangle _ _ rWidth rHeight) <- liftM snd $ liftIO (layoutGetPixelExtents text)
	let xLeft = fromIntegral marginX
	let xCenter = fromIntegral $ (width `div` 2) - (rWidth `div` 2)
	let xRight = fromIntegral $ width - rWidth - marginX
	let yTop = fromIntegral marginY
	let yBottom = fromIntegral $ height - rHeight - marginY
	case position displayItem of
		TopLeft -> moveTo xLeft yTop
		TopCenter -> moveTo xCenter yTop
		TopRight -> moveTo xRight yTop
		BottomLeft -> moveTo xLeft yBottom
		BottomCenter -> moveTo xCenter yBottom
		BottomRight -> moveTo xRight yBottom
	renderText text ctxt textStyle textSizePoints
	return ()

renderText :: PangoLayout -> PangoContext -> TextStyle -> Double -> Render ()
renderText text ctxt textStyle fontSize = do
	liftIO $ updateFontFromTextStyle ctxt text textStyle fontSize
	layoutPath text
	setSourceRGBA `applyColor` textFill textStyle
	fillPreserve
	setSourceRGBA `applyColor` textStroke textStyle
	(Rectangle _ _ _ rHeight) <- liftM snd $ liftIO (layoutGetPixelExtents text)
	setLineWidth $ fromIntegral rHeight * strokeHeightRatio textStyle
	stroke

updateFontFromTextStyle :: PangoContext -> PangoLayout -> TextStyle -> Double -> IO ()
updateFontFromTextStyle ctxt text textStyle fontSize = do
	font <- case fontName textStyle of
		Nothing -> contextGetFontDescription ctxt
		Just name -> fontDescriptionFromString name
	liftIO $ fontDescriptionSetSize font fontSize
	liftIO $ layoutSetFontDescription text (Just font)

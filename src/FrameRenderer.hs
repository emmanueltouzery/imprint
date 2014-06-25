{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module FrameRenderer (renderFrame, renderText, parseFormat,
	FormatElement(..), ImageInfo(..), getTextToRender) where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo hiding (width, height, x, y)
import Control.Monad (liftM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Maybe (fromMaybe)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
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
parseFormatParsec = do
	r <- many1 parseFormatElement
	eof
	return r

parseFormatElement :: GenParser Char st FormatElement
parseFormatElement = try parseDate <|> try parseEscapedPercent <|> try parseFormatItem <|> parseString

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
	(try (string "file") >> return Filename)
		<|> (try (string "expo_bias") >> return (ExifContents exposureBiasValue))
		<|> (try (string "expo") >> return (ExifContents exposureTime))
		<|> (try (string "aper") >> return (ExifContents fnumber))
		<|> (try (string "iso") >> return (ExifContents isoSpeedRatings))
		<|> (try (string "make") >> return (ExifContents make))
		<|> (try (string "model") >> return (ExifContents model))
		<|> (try (string "soft") >> return (ExifContents software))
		<|> (try (string "copy") >> return (ExifContents copyright))
		<|> (try (string "focal35") >> return (ExifContents focalLengthIn35mmFilm))
		<?> "known format item"
	
parseString :: GenParser Char st FormatElement
parseString = do
	contents <- many1 $ noneOf "%"
	return $ StringContents contents

data ImageInfo = ImageInfo
	{
		imgFilename :: String,
		imgExifTags :: Map ExifTag ExifValue
	}

getFormatElementValue :: ImageInfo -> FormatElement -> String
getFormatElementValue imageInfo Filename = imgFilename imageInfo
getFormatElementValue (ImageInfo _ exifTags) (ExifContents tag) =
	maybe "No data" showFunction $ Map.lookup tag exifTags
	where
		showFunction
			| tag `elem` [fnumber, exposureBiasValue] = formatAsFloatingPoint 2
			| otherwise = show
getFormatElementValue (ImageInfo _ exifTags) (DateFormat dateFormatStr) = fromMaybe "No data"
	$ liftM (formatTime defaultTimeLocale dateFormatStr) $ getDateTimeOriginal exifTags
getFormatElementValue _ (StringContents str) = str

getTextToRender :: DisplayItem -> ImageInfo -> String
getTextToRender displayItem imageInfo = foldl' (\s fe -> s ++ getFormatElementValue imageInfo fe) "" formatElements
	where formatElements = case parseFormat $ itemContents displayItem of
		Right c -> c
		-- TODO error in the GUI?
		Left x -> error $ "Invalid item format: " ++ show x

-- TODO not possible to get the current pango context myself?
renderFrame :: Int -> Int -> ImageInfo -> PangoLayout -> PangoContext -> [(DisplayItem, TextStyle)] -> Render ()
renderFrame width height imageInfo text ctxt = mapM_ $ uncurry $ renderDisplayItem width height imageInfo text ctxt

renderDisplayItem :: Int -> Int -> ImageInfo -> PangoLayout -> PangoContext -> DisplayItem -> TextStyle -> Render ()
renderDisplayItem width height imageInfo text ctxt displayItem textStyle = do
	let textToRender = getTextToRender displayItem imageInfo
	liftIO $ layoutSetText text textToRender

	let marginX = floor $ fromIntegral width * marginXFromWidth displayItem
	let marginY = floor $ fromIntegral width * marginYFromWidth displayItem
	let textSizePoints = fromIntegral (max width height) * textSizeFromWidth displayItem
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
	(Rectangle rX rY rWidth rHeight) <- liftM snd $ liftIO (layoutGetPixelExtents text)

	(curX, curY) <- getCurrentPoint
	roundedRect (backBorderRadiusHeightRatio textStyle * fromIntegral rHeight)
		(fromIntegral rX) (fromIntegral rY) (fromIntegral rWidth) (fromIntegral rHeight)
	setSourceRGBA `applyColor` backColor textStyle
	fill
	moveTo curX curY

	layoutPath text
	setSourceRGBA `applyColor` textFill textStyle
	fillPreserve
	setSourceRGBA `applyColor` textStroke textStyle
	setLineWidth $ fromIntegral rHeight * strokeHeightRatio textStyle
	stroke

roundedRect :: Double -> Double -> Double -> Double -> Double -> Render ()
roundedRect cornerRadius x y width height = do
	(curX, curY) <- getCurrentPoint
	-- http://cairographics.org/samples/rounded_rectangle/
	let degrees = pi / 180
	let leftEdge = x + cornerRadius + curX
	let topEdge = y + cornerRadius + curY
	let rightEdge = x + width - cornerRadius + curX
	let bottomEdge = y + height-cornerRadius + curY
	arc rightEdge topEdge cornerRadius (-90*degrees) 0
	arc rightEdge bottomEdge cornerRadius 0 (90*degrees)
	arc leftEdge bottomEdge cornerRadius (90*degrees) (180*degrees)
	arc leftEdge topEdge cornerRadius (180*degrees) (270*degrees)
	closePath

updateFontFromTextStyle :: PangoContext -> PangoLayout -> TextStyle -> Double -> IO ()
updateFontFromTextStyle ctxt text textStyle fontSize = do
	font <- case fontName textStyle of
		Nothing -> contextGetFontDescription ctxt
		Just name -> fontDescriptionFromString name
	liftIO $ fontDescriptionSetSize font fontSize
	liftIO $ layoutSetFontDescription text (Just font)

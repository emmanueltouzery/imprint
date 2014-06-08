module FrameRenderer (renderFrame, renderText, contextSetFontSize) where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo hiding (width, height, x)
import Control.Monad (liftM)

import Settings
import Helpers

-- TODO not possible to get the current pango context myself?
renderFrame :: Int -> Int -> PangoLayout -> PangoContext -> [(DisplayItem, TextStyle)] -> Render ()
renderFrame width height text ctxt = mapM_ $ uncurry $ renderDisplayItem width height text ctxt

renderDisplayItem :: Int -> Int -> PangoLayout -> PangoContext -> DisplayItem -> TextStyle -> Render ()
renderDisplayItem width height text ctxt displayItem textStyle = do
	let marginX = floor $ fromIntegral width * marginXFromWidth displayItem
	let marginY = floor $ fromIntegral width * marginYFromWidth displayItem
	let textSizePoints = fromIntegral width * textSizeFromWidth displayItem
	liftIO $ contextSetFontSize ctxt textSizePoints
	-- renderText will also set the font, but I must do it
	-- before already to get the right font metrics...
	liftIO $ updateFontFromTextStyle ctxt textStyle
	(Rectangle _ _ rWidth rHeight) <- liftM snd $ liftIO (layoutGetPixelExtents text)
	moveTo (fromIntegral $ width - rWidth - marginX)
		(fromIntegral $ height - rHeight - marginY)
	renderText text ctxt textStyle
	return ()

renderText :: PangoLayout -> PangoContext -> TextStyle -> Render ()
renderText text ctxt textStyle = do
	liftIO $ updateFontFromTextStyle ctxt textStyle
	layoutPath text
	setSourceRGBA `applyColor` textFill textStyle
	fillPreserve
	setSourceRGBA `applyColor` textStroke textStyle
	(Rectangle _ _ _ rHeight) <- liftM snd $ liftIO (layoutGetPixelExtents text)
	setLineWidth $ fromIntegral rHeight * strokeHeightRatio textStyle
	strokePreserve

updateFontFromTextStyle :: PangoContext -> TextStyle -> IO ()
updateFontFromTextStyle ctxt textStyle = do
	font <- case fontName textStyle of
		Nothing -> contextGetFontDescription ctxt
		Just name -> liftIO $ fontDescriptionFromString name
	--liftIO $ layoutSetFontDescription text (Just fontDesc)
	contextSetFontDescription ctxt font

contextSetFontSize :: PangoContext -> Double -> IO ()
contextSetFontSize ctxt fontSize = do
	font <- contextGetFontDescription ctxt
	fontDescriptionSetSize font fontSize
	contextSetFontDescription ctxt font

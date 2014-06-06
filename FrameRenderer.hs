module FrameRenderer (renderFrame) where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo hiding (width, height, x)
import Control.Monad (liftM)
import Data.AppSettings (GetSetting(..))

import Settings
import TextStylesSettings

renderFrame :: Int -> Int -> PangoLayout -> GetSetting -> Render ()
renderFrame width height text gs@(GetSetting getSetting) = do
	let marginX = floor $ fromIntegral width * getSetting marginXFromWidth
	let marginY = floor $ fromIntegral width * getSetting marginYFromWidth
	(Rectangle _ _ rWidth rHeight) <- liftM snd $ liftIO (layoutGetPixelExtents text)
	moveTo (fromIntegral $ width - rWidth - marginX)
		(fromIntegral $ height - rHeight - marginY)
	renderText text (getSelectedTextStyle gs)
	return ()

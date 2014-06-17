module Helpers where

import Graphics.UI.Gtk
import Data.Word

rectWidth :: Rectangle -> Int
rectWidth (Rectangle _ _ w _) = w

rectHeight :: Rectangle -> Int
rectHeight (Rectangle _ _ _ h) = h

applyColor :: (Double->Double->Double->Double->a) -> (Double,Double,Double,Double) -> a
applyColor f (r,g,b,a) = f r g b a

getGtkColorNoAlpha :: (Double, Double, Double, Double) -> Color
getGtkColorNoAlpha (r, g, b, _) = Color (convertChannel r) (convertChannel g) (convertChannel b)

convertChannel :: Double -> Word16
convertChannel x = fromIntegral . round $ x*65535

readGtkColorAlpha :: Color -> Word16 -> (Double, Double, Double, Double)
readGtkColorAlpha (Color r g b) alpha = (fromGtkChannel r, fromGtkChannel g, fromGtkChannel b, fromGtkChannel alpha)
	where fromGtkChannel x = (fromIntegral x/65536)

buttonSetColor :: ColorButtonClass self => self -> (Double, Double, Double, Double) -> IO ()
buttonSetColor btn color@(_, _, _, a) = do
	colorButtonSetColor btn $ getGtkColorNoAlpha color
	colorButtonSetAlpha btn $ convertChannel a

displayError :: WindowClass a => a -> String -> IO ()
displayError parent msg = do
	dialog <- messageDialogNew (Just $ toWindow parent) [DialogModal] MessageError ButtonsOk msg
	dialogRun dialog >> widgetDestroy dialog

dialogYesNo :: WindowClass a => a -> String -> IO Bool
dialogYesNo parent msg = do
	dialog <- messageDialogNew (Just $ toWindow parent) [DialogModal] MessageWarning ButtonsYesNo msg
	resp <- dialogRun dialog
	widgetDestroy dialog
	return $ resp == ResponseYes

whenM :: Monad m => m Bool -> m () -> m ()
whenM test action = test >>= \t -> if t then action else return ()

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

module Helpers where

import GHC.Generics
import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Cairo
import Data.Word

rectWidth (Rectangle _ _ w _) = w
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

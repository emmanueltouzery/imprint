{-# LANGUAGE DeriveGeneric #-}

module Helpers where

import Data.Aeson
import GHC.Generics
import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Cairo

data CairoColor = CairoColor
	{
		r :: Double,
		g :: Double,
		b :: Double,
		a :: Double
	} deriving Generic
instance FromJSON CairoColor

rectWidth (Rectangle _ _ w _) = w
rectHeight (Rectangle _ _ _ h) = h

mkCairoColor :: Double -> Double -> Double -> Double -> CairoColor
mkCairoColor r g b a = CairoColor r g b a

applyColor :: (Double->Double->Double->Double->a) -> CairoColor -> a
applyColor f (CairoColor r g b a) = f r g b a

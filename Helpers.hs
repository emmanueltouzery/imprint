module Helpers where

import GHC.Generics
import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Cairo

rectWidth (Rectangle _ _ w _) = w
rectHeight (Rectangle _ _ _ h) = h

applyColor :: (Double->Double->Double->Double->a) -> (Double,Double,Double,Double) -> a
applyColor f (r,g,b,a) = f r g b a

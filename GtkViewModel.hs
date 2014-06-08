{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances #-}

module GtkViewModel (
	Model,
	readModel,
	makeModel,
	modifyModel,
	Bindable,
	bindModel,
	addModelObserver
) where

import Data.IORef
import Control.Lens
import Graphics.UI.Gtk
import Control.Monad (liftM)

import Helpers
import Settings (ColorRgba)

data Model a = Model
	{
		contents :: IORef a,
		changeCallbacks :: IORef [a -> IO ()]
	}

modifyModel :: Model a -> (a->a) -> IO (Model a)
modifyModel model transformer = do
	modifyIORef (contents model) transformer
	newValue <- readModel model
	callbacks <- readIORef $ changeCallbacks model
	mapM_ (\cb -> cb newValue) callbacks
	return model

readModel :: Model a -> IO a
readModel = readIORef . contents

makeModel :: a -> IO (Model a)
makeModel v = do
	value <- newIORef v
	bindings <- newIORef []
	return $ Model value bindings

addModelObserver :: Model a -> (a -> IO ()) -> IO ()
addModelObserver model cb = modifyIORef (changeCallbacks model) $ (cb:)

class Bindable b c where
	setWidgetValue :: b -> c -> IO ()
	registerWidgetListener :: b -> (c -> IO ()) -> IO ()

-- would be nice to have an helper that takes the builder
-- the string and the caster too, though it may be complicated with caster crap.
bindModel :: Bindable b c => Model a -> (Lens' a c) -> b -> IO ()
bindModel model memberLens bindable = do
	let reader = \x -> x ^. memberLens
	modifyIORef (changeCallbacks model) $ (setWidgetValue bindable . reader:)
	readIORef (contents model) >>= setWidgetValue bindable . reader
	registerWidgetListener bindable $ \newV ->
		modifyModel model (memberLens .~ newV) >> return ()

instance Bindable FontButton (Maybe String) where
	setWidgetValue w Nothing = fontButtonSetFontName w "" >> return ()
	setWidgetValue w (Just v) = fontButtonSetFontName w v >> return ()
	registerWidgetListener fontButton cb = do
		onFontSet fontButton $ do
			selectedFontName <- fontButtonGetFontName fontButton
			cb $ Just selectedFontName
		>> return ()

instance Bindable ColorButton ColorRgba where
	setWidgetValue = buttonSetColor
	registerWidgetListener w cb = do
		onColorSet w $ do
			gtkColor <- colorButtonGetColor w
			alpha <- colorButtonGetAlpha w
			cb $ readGtkColorAlpha gtkColor alpha
		>> return ()

instance Bindable Adjustment Double where
	setWidgetValue w v = adjustmentSetValue w $ v*100
	registerWidgetListener w cb = do
		onValueChanged w $ do
			liftM (/100) (adjustmentGetValue w) >>= cb
		>> return ()

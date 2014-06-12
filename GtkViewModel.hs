{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances #-}

module GtkViewModel (
	Model,
	readModel,
	makeModel,
	modifyModel,
	ListModel,
	readListModel,
	makeListModel,
	addListModelAddObserver,
	addListModelRemoveObserver,
	addListModelCurrentItemObserver,
	listModelAddItem,
	listModelRemoveItem,
	listModelSetCurrentItem,
	listModelGetCurrentItem,
	Bindable,
	bindModel,
	addModelObserver,
	RangeBindInfo(..)
) where

import Data.IORef
import Control.Lens
import Graphics.UI.Gtk
import Control.Monad (liftM, when)
import Data.List

import Helpers
import Settings (ColorRgba)

data Model a = Model
	{
		contents :: IORef a,
		changeCallbacks :: IORef [a -> IO ()]
	}
instance Eq (Model a) where
	a == b = contents a == contents b

data ListModel a = ListModel
	{
		items :: IORef [Model a],
		addedCallbacks :: IORef [Model a -> IO()],
		removedCallbacks :: IORef [Model a -> IO()],
		currentItem :: IORef (Maybe (Model a)),
		currentItemCallbacks :: IORef [Model a -> IO()]
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

makeListModel :: [a] -> IO (ListModel a)
makeListModel listItems = do
	itemsModel <- mapM makeModel listItems >>= newIORef
	addedCb <- newIORef []
	removedCb <- newIORef []
	curItem <- newIORef Nothing
	curItemCb <- newIORef []
	return $ ListModel itemsModel addedCb removedCb curItem curItemCb

readListModel :: ListModel a -> IO [Model a]
readListModel = readIORef . items

-- TODO code duplication in next 3 functions
addListModelAddObserver :: Eq a => ListModel a -> (Model a -> IO()) -> IO ()
addListModelAddObserver listModel cb = modifyIORef (addedCallbacks listModel) (cb:)

addListModelRemoveObserver :: Eq a => ListModel a -> (Model a -> IO()) -> IO ()
addListModelRemoveObserver listModel cb = modifyIORef (removedCallbacks listModel) (cb:)

addListModelCurrentItemObserver :: ListModel a -> (Model a -> IO()) -> IO ()
addListModelCurrentItemObserver listModel cb = modifyIORef (currentItemCallbacks listModel) (cb:)

-- TODO code duplication in next 3 functions
listModelRemoveItem :: Eq a => ListModel a -> Model a -> IO ()
listModelRemoveItem listModel itemModel = do
	curItem <- readIORef $ currentItem listModel
	when (curItem == Just itemModel) $
		-- removing the current item!
		modifyIORef (currentItem listModel) $ const Nothing
	modifyIORef (items listModel) $ filter ((/=contents itemModel) . contents)
	readIORef (removedCallbacks listModel) >>= mapM (flip ($) itemModel) >> return ()

listModelAddItem :: ListModel a -> Model a -> IO ()
listModelAddItem listModel itemModel = do
	-- would be faster to add at the beginning, but it's
	-- not what the user expects...
	modifyIORef (items listModel) (\l -> l ++ [itemModel])
	readIORef (addedCallbacks listModel) >>= mapM (flip ($) itemModel) >> return ()

listModelSetCurrentItem :: ListModel a -> Model a -> IO ()
listModelSetCurrentItem listModel item = do
	list <- readIORef (items listModel)
	case find (==item) list of
		x@(Just _) -> do
			modifyIORef (currentItem listModel) $ const x
			readIORef (currentItemCallbacks listModel) >>= mapM (flip ($) item) >> return ()
		Nothing -> error "listModelSetCurrentItem: item not in list!"

listModelGetCurrentItem :: ListModel a -> IO (Maybe (Model a))
listModelGetCurrentItem listModel = readIORef (currentItem listModel)

addModelObserver :: Model a -> (a -> IO ()) -> IO ()
addModelObserver model cb = modifyIORef (changeCallbacks model) (cb:)

class Bindable b c where
	initBind :: b -> c -> IO ()
	initBind _ _ = return ()
	setWidgetValue :: b -> c -> IO ()
	registerWidgetListener :: b -> (c -> IO ()) -> IO ()

-- would be nice to have an helper that takes the builder
-- the string and the caster too, though it may be complicated with caster crap.
bindModel :: Bindable b c => Model a -> (Lens' a c) -> b -> IO ()
bindModel model memberLens bindable = do
	let reader = \x -> x ^. memberLens
	readIORef (contents model) >>= \v -> initBind bindable $ reader v
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

data RangeBindInfo a = RangeBindInfo
	{
		range :: a,
		lowerV :: Double,
		upperV :: Double,
		stepIncr :: Double,
		pageIncr :: Double,
		pageSize :: Double
	}

instance (RangeClass a) => Bindable (RangeBindInfo a) Double where
	initBind bindInfo _ = do
		adj <- adjustmentNew (lowerV bindInfo) (lowerV bindInfo) (upperV bindInfo)
			(stepIncr bindInfo) (pageIncr bindInfo) (pageSize bindInfo)
		rangeSetAdjustment (range bindInfo) adj
	setWidgetValue w v = do
		adj <- rangeGetAdjustment $ range w
		adjustmentSetValue adj $ v*100
	registerWidgetListener w cb = do
		adj <- rangeGetAdjustment $ range w
		onValueChanged adj $ do
			liftM (/100) (adjustmentGetValue adj) >>= cb
		>> return ()

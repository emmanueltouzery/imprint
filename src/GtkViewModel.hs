{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
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
    listModelFind,
    Bindable,
    bindModel,
    addModelObserver,
    RangeBindInfo(..),
    ComboBindInfo(..)
) where

import Data.IORef
import Control.Lens
import Graphics.UI.Gtk
import Control.Monad (when, void)
import Control.Applicative ( (<$>) )
import Data.List
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

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
    readIORef (removedCallbacks listModel) >>= mapM_ ($ itemModel)

listModelAddItem :: ListModel a -> Model a -> IO ()
listModelAddItem listModel itemModel = do
    -- would be faster to add at the beginning, but it's
    -- not what the user expects...
    modifyIORef (items listModel) (\l -> l ++ [itemModel])
    readIORef (addedCallbacks listModel) >>= mapM_ ($ itemModel)

listModelFind :: (a -> Bool) -> ListModel a -> IO (Maybe (Model a))
listModelFind cb listModel = do
    list <- readIORef (items listModel)
    curList <- mapM readModel list
    return $ (!!) list <$> findIndex cb curList

listModelSetCurrentItem :: ListModel a -> Model a -> IO ()
listModelSetCurrentItem listModel item = do
    list <- readIORef (items listModel)
    case find (==item) list of
        x@(Just _) -> do
            modifyIORef (currentItem listModel) $ const x
            readIORef (currentItemCallbacks listModel) >>= mapM_ ($ item)
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
bindModel :: Bindable b c => Model a -> Lens' a c -> b -> IO ()
bindModel model memberLens bindable = do
    let reader = (^. memberLens)
    readIORef (contents model) >>= \v -> initBind bindable $ reader v
    modifyIORef (changeCallbacks model) (setWidgetValue bindable . reader:)
    readIORef (contents model) >>= setWidgetValue bindable . reader
    registerWidgetListener bindable $ \newV ->
        void (modifyModel model (memberLens .~ newV))

instance Bindable FontButton (Maybe String) where
    setWidgetValue w Nothing = void (fontButtonSetFontName w "")
    setWidgetValue w (Just v) = void (fontButtonSetFontName w v)
    registerWidgetListener fontButton cb = void (onFontSet fontButton $ do
            selectedFontName <- fontButtonGetFontName fontButton
            cb $ Just selectedFontName)

instance Bindable ColorButton ColorRgba where
    setWidgetValue = buttonSetColor
    registerWidgetListener w cb = void (onColorSet w $ do
            gtkColor <- colorButtonGetColor w
            alpha <- colorButtonGetAlpha w
            cb $ readGtkColorAlpha gtkColor alpha)

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
    registerWidgetListener w cb = void (do
        adj <- rangeGetAdjustment $ range w
        onValueChanged adj $ (/100) <$> (adjustmentGetValue adj) >>= cb)

-- for the combo bind info, you can give a defaultIndex, which will
-- be used in case the model is set to a value which is not contained
-- in the comboValues.
-- That is useful in combination with comboExtraValues, where you can
-- list combo entries which won't be managed by the binding.
-- Entries in comboExtraValues won't trigger any changes to the model.
-- If you want to do something when they are picked, you must listen
-- to events from the combo by hand.
-- That is useful for instance for combos containing options like..
-- value1, value2, value3, OTHER.
-- OTHER would be in a comboExtraValues and would not be in the
-- comboValues. Then when the user clicks OTHER you could open a dialog,
-- and so on.
--
-- The comboConnectId is to allow us to change the model which is
-- bound to a specific combo. If it's not Nothing, we'll unbind
-- before binding again.
data ComboBindInfo a b = ComboBindInfo
    {
        comboWidget :: a,
        comboValues :: [(String,b)],
        comboExtraValues :: [String],
        defaultIndex :: Int,
        comboConnectId :: IORef (Maybe (ConnectId a))
    }

instance (ComboBoxClass a, Eq b) => Bindable (ComboBindInfo a b) b where
    initBind (ComboBindInfo combo values extraValues _ connectIdRef) _ = do
        connectId <- readIORef connectIdRef
        whenIsJust connectId signalDisconnect
        comboBoxSetModelText combo
        mapM_ (comboBoxAppendText combo . T.pack . fst) values
        mapM_ (comboBoxAppendText combo . T.pack) extraValues
    setWidgetValue (ComboBindInfo combo values _ defaultIdx _) curValue = do
        let idx = fromMaybe defaultIdx $ findIndex ((==curValue) . snd) values
        comboBoxSetActive combo idx
    registerWidgetListener (ComboBindInfo combo values _ _ connectIdRef) cb = do
        cId <- combo `on` changed $ do
            idx <- comboBoxGetActive combo
            when (idx < length values) $ do
                let selectedValue = snd $ values !! idx
                cb selectedValue
        modifyIORef connectIdRef $ const (Just cId)

instance Bindable Entry String where
    setWidgetValue = entrySetText
    registerWidgetListener w cb = void (w `on` editableChanged $ entryGetText w >>= cb)

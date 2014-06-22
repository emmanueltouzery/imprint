module Helpers where

import Graphics.UI.Gtk
import Data.Word
import Data.IORef
import Data.Maybe (isJust)
import qualified Data.Map as Map
import Data.Map (Map)

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

data BuilderHolder = BuilderHolder
	{
		boundBuilder :: Builder,
		buttonBinders :: IORef (Map String ButtonBinder)
	}

getBuilderHolder :: Builder -> IO BuilderHolder
getBuilderHolder builder = do
	binders <- newIORef $ Map.fromList []
	return $ BuilderHolder
		{
			boundBuilder = builder,
			buttonBinders = binders
		}

-- The problem this solves is that I have dialogs
-- that I show and hide and show again, because
-- I use the gtkbuilder system.
-- And when I show it again, I must disconnect
-- the previous button click handlers before I
-- connect the new one, otherwise the old handler
-- also gets invoked. To disconnect I need the
-- connection ID, that I must store...
-- TODO move to this pattern in more places
builderHolderGetButtonBinder :: BuilderHolder -> String -> IO (ButtonBinder)
builderHolderGetButtonBinder builderHolder btnName = do
	bindersV <- readIORef $ buttonBinders builderHolder
	case Map.lookup btnName bindersV of
		Just btnBinder -> return btnBinder
		Nothing -> do
			btn <- builderGetObject (boundBuilder builderHolder) castToButton btnName
			cb <- newIORef Nothing
			let binder = ButtonBinder btn cb
			modifyIORef (buttonBinders builderHolder)
				$ const $ Map.insert btnName binder bindersV
			return binder

data ButtonBinder = ButtonBinder
	{
		boundButton :: Button,
		currentCbId :: IORef (Maybe (ConnectId Button))
	}

buttonBindCallback :: ButtonBinder -> IO () -> IO ()
buttonBindCallback btnBinder cb = do
	cbId <- readIORef $ currentCbId btnBinder
	readIORef (currentCbId btnBinder) >>= print . isJust
	whenIsJust cbId $ signalDisconnect
	newCbId <- (boundButton btnBinder) `on` buttonActivated $ cb
	modifyIORef (currentCbId btnBinder) $ const (Just newCbId)
	readIORef (currentCbId btnBinder) >>= print . isJust

whenM :: Monad m => m Bool -> m () -> m ()
whenM test action = test >>= \t -> if t then action else return ()

-- TODO move to the Data.Either implementation present in base 4.7.0
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

whenIsJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenIsJust mA s = case mA of
	Nothing -> return ()
	Just x -> s x

{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.Rendering.Cairo hiding (width, height, x)
import Graphics.UI.Gtk hiding (styleSet)
import Graphics.HsExif (parseFileExif)
import Data.IORef
import Control.Monad (liftM, when)
import Data.List
import Data.Maybe (fromJust, isJust)
import Data.AppSettings
import Control.Concurrent (forkOS)
import Text.Printf (printf)
import System.FilePath.Posix (splitFileName, pathSeparator)
import Control.Exception (try, SomeException)
import System.GIO.File.File (filePath, fileFromURI)
import Data.ByteString.UTF8 (toString)
import System.Directory (createDirectoryIfMissing)

import Settings
import SettingsDialog
import FrameRenderer (renderFrame, ImageInfo(..))
import Helpers (isLeft, displayError)

minFontSize :: Int
minFontSize = 5

main :: IO ()
main = do
	initGUI

	-- TODO this must go in a try
	-- This is the settings dialog.
	-- Therefore it's a special situation because
	-- the settings can change anytime.
	-- in the rest of the app however they'll be static.
	settings <- liftM fst Settings.readSettings

	-- ############ TODO I think i don't need to hold the config in an IORef
	-- now. I want realtime edit when previewing changes in the OK/Cancel dialog.
	-- But the rest of the time I'll in fact reload. So I think I can limit the
	-- IORef to the scope of the OK/Cancel preview dialog.
	latestConfig <- newIORef settings

	builder <- builderNew
	builderAddFromFile builder "imprint.ui"

	Settings.saveSettings settings

	showMainWindow builder latestConfig

	mainGUI

showMainWindow :: Builder -> IORef Conf -> IO ()
showMainWindow builder latestConfig = do
	mainWindow <- builderGetObject builder castToWindow "main_window"

	mainWindow `on` objectDestroy $ mainQuit

	settingsDialog <- prepareSettingsDialog builder latestConfig
	set settingsDialog [windowTransientFor := mainWindow]

	toolbarPreferences <- builderGetObject builder castToToolButton "toolbarPreferences"
	onToolButtonClicked toolbarPreferences $ do
		dialogRun settingsDialog
		widgetHide settingsDialog

	imprintDropDestination <- builderGetObject builder castToLabel "imprint_drop_destination"
	dragDestSet imprintDropDestination [DestDefaultMotion, DestDefaultDrop] [ActionCopy]
	dragDestAddURITargets imprintDropDestination

	imprintDropDestination `on` dragDataReceived $ \dragCtxt _ _ time ->
		dragReceived dragCtxt time builder mainWindow latestConfig

	windowSetDefaultSize mainWindow 600 500
	widgetShowAll mainWindow

dragReceived :: DragContext -> TimeStamp -> Builder -> Window -> IORef Conf -> SelectionDataM ()
dragReceived dragCtxt time builder mainWindow latestConfig = do
	mUris <- selectionDataGetURIs
	let success = True
	let deleteOriginal = False -- True for a move.
	liftIO $ do
		dragFinish dragCtxt success deleteOriginal time
		when (isJust mUris) $ do
			settings <- readIORef latestConfig
			userCancel <- newIORef False -- must move to MVar...
			progressDialog <- builderGetObject builder castToDialog "progressDialog"
			progressLabel <- builderGetObject builder castToLabel "progressLabel"
			progressBar <- builderGetObject builder castToProgressBar "progressBar"
			progressCancel <- builderGetObject builder castToButton "progressCancel"
			progressCancel `on` buttonActivated $ modifyIORef userCancel $ const True
			let filenames = map filenameFromUri $ fromJust mUris
			forkOS $ convertPictures filenames settings 
				progressDialog mainWindow progressLabel progressBar userCancel
			set progressDialog [windowTransientFor := mainWindow]
			dialogRun progressDialog >> return ()

filenameFromUri :: String -> String
filenameFromUri = toString . filePath . fileFromURI

-- Careful this is in another thread...
convertPictures :: [String] -> Conf -> Dialog -> Window -> Label -> ProgressBar -> IORef Bool -> IO ()
convertPictures files settings dialog mainWindow label progressbar userCancel = do
	print "convert pictures!"
	result <- mapM (uncurry $ convertPicture settings label progressbar (length files) userCancel) $ zip files [1..]
	let errors = filter (isLeft . snd) result
	when (not $ null errors) $ do
		let listOfFiles = intercalate "\n" $ map fst errors
		let firstErrorMsg = show $ (\(Left a) -> a) $ snd $ head errors
		let msg = printf "%d errors during processing, the first error was:\n\n%s\n\nThe list of files that failed is:\n%s"
				(length errors) firstErrorMsg listOfFiles
		print msg
		postGUIAsync $ displayError mainWindow msg
	postGUIAsync $ widgetHide dialog
	return ()

-- Careful this is in another thread...
convertPicture :: Conf -> Label -> ProgressBar -> Int -> IORef Bool -> String -> Int -> IO ((String, Either SomeException ()))
convertPicture settings label progressBar filesCount userCancel filename fileIdx = do
	postGUIAsync $ do
		labelSetText label $ printf "Processing image %d/%d" fileIdx filesCount
		progressBarSetFraction progressBar $ (fromIntegral fileIdx) / (fromIntegral filesCount)

	result <- try $ convertPictureImpl settings filename $ getTargetFileName filename

	return (filename, result)

convertPictureImpl :: Conf -> String -> String -> IO ()
convertPictureImpl settings filename targetFilename = do
	exifInfo <- parseFileExif filename
	let exifData = case exifInfo of
		Left errorStr -> error errorStr
		Right exif -> exif

	img <- pixbufNewFromFile filename
	width <- pixbufGetWidth img
	height <- pixbufGetHeight img
	sur <- createImageSurface FormatRGB24 width height
	ctxt <- cairoCreateContext Nothing
	text <- layoutEmpty ctxt

	let imageInfo = ImageInfo filename exifData

	renderWith sur $ do
		setSourcePixbuf img 0 0
		paint
		renderFrame width height imageInfo text ctxt $ getDisplayItemsStylesConf settings

	pbuf <- pixbufNewFromSurface sur 0 0 width height

	let targetFolder = fst $ splitFileName targetFilename
	createDirectoryIfMissing True targetFolder

	pixbufSave pbuf targetFilename "jpeg" [("quality", "95")]

getTargetFileName :: FilePath -> FilePath
getTargetFileName inputFilename = intercalate [pathSeparator] [folder, "imprint", filename]
	where
		(folder, filename) = splitFileName inputFilename

getDisplayItemsStylesConf :: Conf -> [(DisplayItem, TextStyle)]
getDisplayItemsStylesConf conf = zip displayItemsV textStylesV
	where
		displayItemsV = getSetting' conf displayItems
		allTextStylesV = getSetting' conf textStyles
		textStyleById sId = find ((==sId) . styleId) allTextStylesV
		textStylesV = fmap (fromJust . textStyleById  . textStyleId) displayItemsV

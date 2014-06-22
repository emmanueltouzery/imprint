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
import System.FilePath.Posix (splitFileName, pathSeparator, splitPath)
import Control.Exception (try, SomeException)
import System.GIO.File.File (filePath, fileFromURI)
import Data.ByteString.UTF8 (toString)
import System.Directory (createDirectoryIfMissing)
import qualified Data.Function as F (on)

import Settings
import SettingsDialog
import FrameRenderer (renderFrame, ImageInfo(..))
import Helpers

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

	builderHolder <- getBuilderHolder builder

	imprintDropDestination `on` dragDataReceived $ \dragCtxt _ _ time ->
		dragReceived dragCtxt time builderHolder mainWindow latestConfig

	windowSetDefaultSize mainWindow 600 500
	widgetShowAll mainWindow

dragReceived :: DragContext -> TimeStamp -> BuilderHolder -> Window -> IORef Conf -> SelectionDataM ()
dragReceived dragCtxt time builderHolder mainWindow latestConfig = do
	let builder = boundBuilder builderHolder
	mUris <- selectionDataGetURIs
	let success = True
	let deleteOriginal = False -- True for a move.
	liftIO $ do
		dragFinish dragCtxt success deleteOriginal time
		when (isJust mUris) $ do
			-- TODO these URIs can contain folders!! Must expand them
			-- to files by recursively browsing...
			settings <- readIORef latestConfig
			userCancel <- newIORef False -- should move to MVar?...
			progressDialog <- builderGetObject builder castToDialog "progressDialog"
			progressLabel <- builderGetObject builder castToLabel "progressLabel"
			progressBar <- builderGetObject builder castToProgressBar "progressBar"
			progressCancel <- builderGetObject builder castToButton "progressCancel"
			progressClose <- builderGetObject builder castToButton "progressClose"
			progressOpenTargetFolder <- builderHolderGetButtonBinder
				builderHolder "progressOpenTargetFolder"
			widgetSetSensitive (boundButton progressOpenTargetFolder) False
			widgetShow progressCancel
			widgetHide progressClose
			errorsTreeview <- builderGetObject builder castToTreeView "errorsTreeview"
			treeViewGetColumns errorsTreeview >>= mapM_ (treeViewRemoveColumn errorsTreeview)
			errorsStore <- listStoreNew [ ErrorInfo "-" "Started the processing..."]
			treeViewSetModel errorsTreeview errorsStore
			treeViewSetFixedHeightMode errorsTreeview False
			treeViewAddColumn errorsTreeview "Filename" errorsStore $ \(ErrorInfo p _) -> p
			treeViewAddColumn errorsTreeview "Details" errorsStore $ \(ErrorInfo _ d) -> d
			progressClose `on` buttonActivated $ widgetHide progressDialog
			progressCancel `on` buttonActivated $ atomicWriteIORef userCancel True
			let filenames = map filenameFromUri $ fromJust mUris
			let filesCount = length filenames
			let targetFolder = getTargetFolder filenames
			buttonBindCallback progressOpenTargetFolder $ openFolder targetFolder

			let pictureConvertCb = \fileIdx successInfo -> do
				postGUIAsync $ do
					labelSetText progressLabel $ printf "Processing image %d/%d" fileIdx filesCount
					progressBarSetFraction progressBar $ (fromIntegral fileIdx) / (fromIntegral filesCount)
				case successInfo of
					Right _ -> postGUIAsync $ widgetSetSensitive (boundButton progressOpenTargetFolder) True
					Left errorInfo -> postGUIAsync $ listStoreAppend errorsStore errorInfo >> return ()

			forkOS $ convertPictures filenames targetFolder settings userCancel pictureConvertCb $
				postGUIAsync $ do
					labelSetText progressLabel "Finished."
					listStoreAppend errorsStore $ ErrorInfo "-" "Finished the processing."
					widgetHide progressCancel
					widgetShow progressClose
			set progressDialog [windowTransientFor := mainWindow]
			windowSetDefaultSize progressDialog 600 380
			dialogRun progressDialog
			widgetHide progressDialog

treeViewAddColumn :: TreeView -> String -> ListStore a -> (a -> String) -> IO ()
treeViewAddColumn treeView colName treeModel modelToStr = do
	newCol <- treeViewColumnNew
	set newCol [ treeViewColumnTitle := colName,
			  treeViewColumnResizable := True ]
	treeViewAppendColumn treeView newCol
	newRenderer <- cellRendererTextNew
	set newRenderer [ cellTextWrapMode := WrapPartialWords, cellTextWrapWidth := 250]
	cellLayoutPackStart newCol newRenderer True
	treeViewColumnSetSizing newCol TreeViewColumnFixed
	treeViewColumnSetFixedWidth newCol 250
	treeViewColumnSetExpand newCol True
	cellLayoutSetAttributes newCol newRenderer treeModel
		$ \modelV ->  [cellText := modelToStr modelV]

filenameFromUri :: String -> String
filenameFromUri = toString . filePath . fileFromURI

data ErrorInfo = ErrorInfo
	{
		path :: String,
		errorMessage :: String
	} deriving (Show)

-- Careful this is in another thread...
convertPictures :: [String] -> String -> Conf -> IORef Bool -> (Int -> Either ErrorInfo () -> IO ()) -> IO () -> IO ()
convertPictures files targetFolder settings userCancel pictureConvertedCb doneCb = do
	mapM_ (uncurry $ convertPicture settings targetFolder userCancel pictureConvertedCb) $ zip files [1..]
	doneCb

-- TODO make it actually open the folder..
openFolder :: FilePath -> IO ()
openFolder folderPath = putStrLn $ "TODO opening folder " ++ folderPath

-- the user may have dropped a whole file hierarchy like
-- Pictures, Pictures/2014, Pictures/2013 and so on.
-- I don't want to fill in tons of imprint folders all
-- over the place like Pictures/imprint, Pictures/2014/imprint,
-- Pictures/2013/imprint and so on.
-- In this case I want only Pictures/imprint.
--
-- So I must find the "root" folder of the selected files
-- and that's the parent folder for my output imprint folder.
getTargetFolder :: [String] -> String
getTargetFolder files = rootFolder ++ "/imprint"
	where
		pathDepths = map (length . splitPath) files
		filesWithpathDepth = zip files pathDepths
		fileInHigherFolder = fst $ head $ sortBy (compare `F.on` snd) filesWithpathDepth
		rootFolder = fst $ splitFileName fileInHigherFolder

-- Careful this is in another thread...
convertPicture :: Conf -> String -> IORef Bool -> (Int -> Either ErrorInfo () -> IO ()) -> String -> Int -> IO ()
convertPicture settings targetFolder userCancel pictureConvertedCb filename fileIdx = do
	isUserCancel <- readIORef userCancel
	let logError = pictureConvertedCb fileIdx . Left . ErrorInfo filename
	if isUserCancel
		then logError "User cancelled"
		else do
			(result :: Either SomeException ()) <- try $
				convertPictureImpl settings filename $ getTargetFileName filename targetFolder
			if (isLeft result)
				then logError $ show $ (\(Left a) -> a) result
				else pictureConvertedCb fileIdx $ Right ()


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

getTargetFileName :: FilePath -> FilePath -> FilePath
getTargetFileName inputFilename targetFolder = intercalate [pathSeparator] [targetFolder, filename]
	where
		filename = snd $ splitFileName inputFilename

getDisplayItemsStylesConf :: Conf -> [(DisplayItem, TextStyle)]
getDisplayItemsStylesConf conf = zip displayItemsV textStylesV
	where
		displayItemsV = getSetting' conf displayItems
		allTextStylesV = getSetting' conf textStyles
		textStyleById sId = find ((==sId) . styleId) allTextStylesV
		textStylesV = fmap (fromJust . textStyleById  . textStyleId) displayItemsV

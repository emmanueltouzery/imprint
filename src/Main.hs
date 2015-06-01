{-# LANGUAGE ScopedTypeVariables, CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.Rendering.Cairo hiding (width, height, x)
import Graphics.UI.Gtk hiding (styleSet)
import Graphics.HsExif
import Data.IORef
import Control.Monad (void, when)
import Data.List
import Data.Maybe (fromJust)
import Data.Either
import Data.AppSettings
import Control.Concurrent (forkOS)
import Text.Printf (printf)
import qualified Data.Map as Map (empty)
import qualified Data.Text as T
#ifdef CABAL_OS_WINDOWS
import System.FilePath.Windows (splitFileName, pathSeparator)
#else
import System.FilePath.Posix (splitFileName, pathSeparator)
#endif
import Control.Exception (try, SomeException)
import System.GIO.File.File (filePath, fileFromURI)
import Data.ByteString.UTF8 (toString)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents)
import Control.Applicative
import System.Process (rawSystem, runCommand)

import Text.I18N.GetText
#ifdef CABAL_OS_WINDOWS
import WinSupport (setupGetTextWindows)
#else
import System.Locale.SetLocale (setLocale, Category(LC_ALL))
#endif

#ifdef CABAL_OS_WINDOWS
import WinSupport (getDataFileName)
#else
import Paths_imprint (getDataFileName)
#endif

import Settings
import SettingsDialog
import FrameRenderer (renderFrame, ImageInfo(..))
import Helpers

minFontSize :: Int
minFontSize = 5

main :: IO ()
main = do
#ifndef CABAL_OS_WINDOWS
	setLocale LC_ALL (Just "")
	bindTextDomain __MESSAGE_CATALOG_DOMAIN__ (Just __MESSAGE_CATALOG_DIR__)
	textDomain $ Just __MESSAGE_CATALOG_DOMAIN__
#else
	setupGetTextWindows
#endif

	initGUI

	-- TODO this must go in a try
	-- This is the settings dialog.
	-- Therefore it's a special situation because
	-- the settings can change anytime.
	-- in the rest of the app however they'll be static.
	settings <- fst <$> Settings.readSettings

	-- ############ TODO I think i don't need to hold the config in an IORef
	-- now. I want realtime edit when previewing changes in the OK/Cancel dialog.
	-- But the rest of the time I'll in fact reload. So I think I can limit the
	-- IORef to the scope of the OK/Cancel preview dialog.
	latestConfig <- newIORef settings

	builder <- builderNew
	getDataFileName "imprint.ui" >>= builderAddFromFile builder

	Settings.saveSettings settings

	showMainWindow builder latestConfig

	mainGUI

showMainWindow :: Builder -> IORef Conf -> IO ()
showMainWindow builder latestConfig = do
	mainWindow <- builderGetObject builder castToWindow "main_window"

	mainWindow `on` objectDestroy $ mainQuit

	settingsDialog <- prepareSettingsDialog builder latestConfig

	toolbarPreferences <- builderGetObject builder castToToolButton "toolbarPreferences"
	onToolButtonClicked toolbarPreferences $ showDialog settingsDialog mainWindow

	toolbarAbout <- builderGetObject builder castToToolButton "toolbarAbout"
	onToolButtonClicked toolbarAbout $ showAboutDialog mainWindow

	imprintDropDestination <- builderGetObject builder castToLabel "imprint_drop_destination"
	dragDestSet imprintDropDestination [DestDefaultMotion, DestDefaultDrop] [ActionCopy]
	dragDestAddURITargets imprintDropDestination

	builderHolder <- getBuilderHolder builder

	imprintDropDestination `on` dragDataReceived $ \dragCtxt _ _ time ->
		dragReceived dragCtxt time builderHolder mainWindow latestConfig

	windowSetDefaultSize mainWindow 600 500
	widgetShowAll mainWindow

showAboutDialog :: Window -> IO ()
showAboutDialog mainWindow = do
	logoFname <- getDataFileName "imprint-128.png"
	logoPixbuf <- pixbufNewFromFileAtSize logoFname 128 128
	aboutDlg <- aboutDialogNew
	set aboutDlg [aboutDialogProgramName:= "Imprint",
			aboutDialogLicense  := Just "BSD license",
			aboutDialogWebsite  := "https://github.com/emmanueltouzery/imprint/",
			aboutDialogComments := __ "Add information about pictures as text painted on the pictures themselves.",
			aboutDialogLogo     := Just logoPixbuf]
	showDialog aboutDlg mainWindow

dragReceived :: DragContext -> TimeStamp -> BuilderHolder -> Window -> IORef Conf -> SelectionDataM ()
dragReceived dragCtxt time builderHolder mainWindow latestConfig = do
	mUris <- selectionDataGetURIs
	let success = True
	let deleteOriginal = False -- True for a move.
	liftIO $ do
		dragFinish dragCtxt success deleteOriginal time
		whenIsJust mUris $ processDrop builderHolder latestConfig mainWindow

processDrop :: BuilderHolder -> IORef Conf -> Window -> [String] -> IO ()
processDrop builderHolder latestConfig mainWindow uris = do
	let builder = boundBuilder builderHolder
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
	errorsStore <- prepareErrorsTreeView errorsTreeview
	progressClose `on` buttonActivated $ widgetHide progressDialog
	progressCancel `on` buttonActivated $ atomicWriteIORef userCancel True
	let filenames = map filenameFromUri uris
	expandedFilenames <- expandFilenames filenames
	let filesCount = length expandedFilenames
	let targetFolder = getTargetFolder expandedFilenames
	buttonBindCallback progressOpenTargetFolder $ openFolder targetFolder

	let pictureConvertCb fileIdx successInfo = postGUIAsync $ do
		labelSetText progressLabel $ T.pack $ printf "Processing image %d/%d" fileIdx filesCount
		progressBarSetFraction progressBar $ fromIntegral fileIdx / fromIntegral filesCount
		case successInfo of
			Right _ -> widgetSetSensitive (boundButton progressOpenTargetFolder) True
			Left errorInfo -> void (listStoreAppend errorsStore errorInfo)

	forkOS $ convertPictures expandedFilenames targetFolder settings userCancel pictureConvertCb $
		postGUIAsync $ do
			labelSetText progressLabel $ __ "Finished."
			listStoreAppend errorsStore $ ErrorInfo "-" $ __ "Finished the processing."
			widgetHide progressCancel
			widgetShow progressClose
	windowSetDefaultSize progressDialog 600 380
	showDialog progressDialog mainWindow

prepareErrorsTreeView :: TreeView -> IO (ListStore ErrorInfo)
prepareErrorsTreeView errorsTreeview = do
	treeViewGetColumns errorsTreeview >>= mapM_ (treeViewRemoveColumn errorsTreeview)
	errorsStore <- listStoreNew [ ErrorInfo "-" $ __ "Started the processing..."]
	treeViewSetModel errorsTreeview errorsStore
	treeViewSetFixedHeightMode errorsTreeview False
	treeViewAddColumn errorsTreeview "Filename" errorsStore $ \(ErrorInfo p _) -> p
	treeViewAddColumn errorsTreeview "Details" errorsStore $ \(ErrorInfo _ d) -> d
	return errorsStore

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

-- take a list of paths which can be either files or folders,
-- keep the files, but replace the folders by all the files
-- under them (recursively)
expandFilenames :: [String] -> IO [String]
expandFilenames [] = return []
expandFilenames (x:xs) = do
	isDir <- doesDirectoryExist x
	if not isDir
		then (x:) <$> expandFilenames xs
		else do
			filesInDir <- filter (`notElem` [".", ".."]) <$> getDirectoryContents x
			let filesInDirFullPath = map (\f -> x ++ [pathSeparator] ++ f) filesInDir
			(++) <$> expandFilenames filesInDirFullPath <*> expandFilenames xs

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

openFolder :: FilePath -> IO ()
openFolder folderPath = if pathSeparator == '/'
			then void (rawSystem "xdg-open" [folderPath]) -- linux
			else void (runCommand $ "start \"\" \"" ++ folderPath ++ "\"") -- windows


-- Careful this is in another thread...
convertPicture :: Conf -> String -> IORef Bool -> (Int -> Either ErrorInfo () -> IO ()) -> String -> Int -> IO ()
convertPicture settings targetFolder userCancel pictureConvertedCb filename fileIdx = do
	isUserCancel <- readIORef userCancel
	let logError = pictureConvertedCb fileIdx . Left . ErrorInfo filename
	if isUserCancel
		then logError $ __ "User cancelled"
		else do
			(result :: Either SomeException ()) <- try $
				convertPictureImpl settings filename $ getTargetFileName filename targetFolder
			if isLeft result
				then logError $ show $ (\(Left a) -> a) result
				else pictureConvertedCb fileIdx $ Right ()


convertPictureImpl :: Conf -> String -> String -> IO ()
convertPictureImpl settings filename targetFilename = do
	exifInfo <- parseFileExif filename
	let exifData = case exifInfo of
		Left _ -> Map.empty
		Right exif -> exif

	img <- pixbufNewFromFile filename
	width <- pixbufGetWidth img
	height <- pixbufGetHeight img
	let (rotationAngle, newDimensions) = case getOrientation exifData of
		Just (Rotation MinusNinety) -> (pi/2, (height, width))
		Just (Rotation Ninety) -> (-pi/2, (height, width))
		Just (Rotation HundredAndEighty) -> (pi, (width, height))
		_ -> (0, (width, height))

	sur <- uncurry (createImageSurface FormatRGB24) newDimensions
	ctxt <- cairoCreateContext Nothing
	text <- layoutEmpty ctxt

	let imageInfo = ImageInfo filename exifData

	renderWith sur $ do
		save
		let (w, h) = newDimensions
		when (rotationAngle /= 0) $ do
			translate (fromIntegral $ w `div` 2) (fromIntegral $ h `div` 2)
			rotate rotationAngle
			translate (fromIntegral $ -h `div` 2) (fromIntegral $ -w `div` 2)
		setSourcePixbuf img 0 0
		paint
		restore
		renderFrame w h imageInfo text ctxt $ getDisplayItemsStylesConf settings

	pbuf <- uncurry (pixbufNewFromSurface sur 0 0) newDimensions

	let targetFolder = fst $ splitFileName targetFilename
	createDirectoryIfMissing True targetFolder

	pixbufSave pbuf targetFilename (T.pack "jpeg") [("quality", "95")]

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

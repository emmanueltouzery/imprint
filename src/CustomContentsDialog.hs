{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module CustomContentsDialog where

import Data.Maybe (isJust, fromJust)
import Data.List
import Control.Monad (liftM)
import Graphics.UI.Gtk
import Control.Lens hiding (set)

import SettingsWindowData
import GtkViewModel
import FrameRenderer
import Settings
import Helpers

completionDataDate :: [(String, String)]
completionDataDate = [
	("Date formatted in your system language", "%x"),
	("Hours and minutes", "%R"),
	("Hours, minutes and seconds", "%X"),
	("Hour of the day (24h). 00-23", "%H"),
	("Hour of the day (24h). 0-23", "%k"),
	("Hour of half-day (12h). 01-12", "%I"),
	("Hour of half-day (12h). 1-12", "%l"),
	("Minute of hour, 00-59", "%M"),
	("Second of minute, 00-60", "%S"),
	("Year", "%Y"),
	("Year of century, 00-99", "%y"),
	("Month name, long form, January-December", "%B"),
	("Month name, short form, Jan-Dec", "%b"),
	("Month of year, 01-12", "%m"),
	("Day of month, 01-31", "%d"),
	("Day of month, 1-31", "%e"),
	("Day of week, short form, Sun-Sat", "%a"),
	("Day of week, long form, Sunday-Saturday", "%A")
	]

data CompletionRecordType = NormalCompletion | DateCompletion
	deriving (Eq, Show)

data CompletionRecord = CompletionRecord
	{
		complRecordType :: CompletionRecordType,
		complRecordDesc :: String,
		complRecordValue :: String
	} deriving Show

isDateRecord :: CompletionRecord -> Bool
isDateRecord (CompletionRecord t _ _) = t == DateCompletion

showCustomContentsDialog :: WindowClass a => a -> BuilderHolder -> Model DisplayItem -> IO ()
showCustomContentsDialog parent builderHolder displayItemModel = do
	let builder = boundBuilder builderHolder
	dialog <- builderGetObject builder castToDialog "customContentsDialog"
	set dialog [windowTransientFor := parent]
	customContentsEntry <- builderGetObject builder castToEntry "customContentsEntry"
	curContents <- liftM itemContents $ readModel displayItemModel

	okBtnBinder <- builderHolderGetButtonBinder builderHolder "customContentsOK"
	let okBtn = boundButton okBtnBinder
	cancelBtn <- builderGetObject builder castToButton "customContentsCancel"

	defaultDisplayItem <- readModel displayItemModel
	parseResultLabel <- builderGetObject builder castToLabel "parseResultLabel"
	customContentsEntry `on` editableChanged $ do
		text <- entryGetText customContentsEntry
		let isParseOk = not $ isLeft $ parseFormat text 
		labelSetMarkup parseResultLabel $ if isParseOk
			then getTextToRender (defaultDisplayItem {itemContents = text}) fakeImageInfo
			else "<span color='red'><b>Incorrect syntax</b></span>"
		widgetSetSensitivity okBtn isParseOk
	entrySetText customContentsEntry curContents

	completion <- entryCompletionNew
	let completionData = fmap (uncurry $ CompletionRecord NormalCompletion) completionComboData 
				++ [CompletionRecord NormalCompletion "% sign" "%%"] 
				++ fmap (uncurry $ CompletionRecord DateCompletion) completionDataDate
				++ [CompletionRecord DateCompletion "% sign" "%%"] 
	completionStore <- listStoreNew completionData
	entryCompletionSetModel completion $ Just completionStore
	cellValue <- cellRendererTextNew
	cellLayoutPackStart completion cellValue True
	cellLayoutSetAttributes completion cellValue completionStore
		(\val -> [cellText := complRecordDesc val])
	cellDesc <- cellRendererTextNew
	cellLayoutPackStart completion cellDesc True
	cellLayoutSetAttributes completion cellDesc completionStore
		(\val -> [cellText := complRecordValue val])
	entryCompletionSetMinimumKeyLength completion 1
	entryCompletionSetMatchFunc completion $
		customContentsCompletionCb completionData customContentsEntry
	entrySetCompletion customContentsEntry completion
	completion `on` matchSelected $ \_ iter -> do
		let candidate = completionData !! listStoreIterToIndex iter
		textSoFar <- entryGetText customContentsEntry
		cursorPosBefore <- get customContentsEntry entryCursorPosition
		let (beforeCursor, afterCursor) = splitAt cursorPosBefore textSoFar
		textWhichGotCompleted <- liftM fromJust $ textBeforeCursorFromSymbol "%" customContentsEntry
		let lengthBeforeCompletion = length beforeCursor - length textWhichGotCompleted
		let newText = take lengthBeforeCompletion textSoFar
			++ complRecordValue candidate ++ afterCursor
		entrySetText customContentsEntry newText
		let newPos = lengthBeforeCompletion + length (complRecordValue candidate)
		editableSetPosition customContentsEntry newPos
		return True

	buttonBindCallback okBtnBinder $ do
		newText <- entryGetText customContentsEntry
		modifyModel displayItemModel $ itemContentsL .~ newText
		widgetHide dialog
	cancelBtn `on` buttonActivated $ widgetHide dialog

	dialogRun dialog
	widgetHide dialog

textBeforeCursorFromSymbol :: String -> Entry -> IO (Maybe String)
textBeforeCursorFromSymbol symbol entry = do
	cursorPos <- get entry entryCursorPosition
	typed <- liftM (take cursorPos) $ entryGetText entry
	return $ find (isPrefixOf symbol) $ tail $ reverse $ tails typed

customContentsCompletionCb :: [CompletionRecord] -> Entry -> String -> TreeIter -> IO Bool
customContentsCompletionCb completionModel entry _ iter = do
	let candidate = completionModel !! listStoreIterToIndex iter
	beforePercent <- textBeforeCursorFromSymbol "%" entry
	beforeDate <- textBeforeCursorFromSymbol "%date{" entry
	let inDateContext = isJust beforeDate && not ("}" `isInfixOf` fromJust beforeDate)
	case beforePercent of
		Nothing -> return False
		Just fromPercent | inDateContext ->
			return $ isDateRecord candidate && fromPercent `isPrefixOf` complRecordValue candidate
		Just fromPercent -> 
			return $ not (isDateRecord candidate) && fromPercent `isPrefixOf` complRecordValue candidate

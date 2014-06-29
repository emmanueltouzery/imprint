-- this module is only used on windows.
module WinSupport where

import System.FilePath.Windows (splitFileName)
import System.Environment (getExecutablePath)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (unless, void, liftM)
import Foreign.C.Types
import Foreign.C.String
import GHC.Windows
import Text.I18N.GetText

import Helpers

getDataFileName :: String -> IO String
getDataFileName fname = do
	appFolder <- liftM (fst . splitFileName) getExecutablePath
	return $ appFolder ++ fname

setupGetTextWindows :: IO ()
setupGetTextWindows = do
	setEnv_ "OUTPUT_CHARSET" "utf8"
	-- gettext checks $LANG but it's not what windows uses...
	-- find out the language from windows and write the
	-- $LANG so that the app starts in the correct language...
	winLang <- liftM fromIntegral getUserDefaultUILanguage
	let locale = Map.lookup winLang localesMap
	whenIsJust locale $ \localeStr -> do
		putEnv "OUTPUT_CHARSET=utf8"
		setEnv_ "LANG" localeStr --"fr_FR.UTF8"
		putEnv $ "LANG=" ++ localeStr -- fr_FR.UTF8"
	
	translationsFolder <- getDataFileName "po"
	bindTextDomain "imprint" (Just translationsFolder)
	textDomain $ Just "imprint"
	return ()

localesMap :: Map Int String
localesMap = Map.fromList [
	(1036, "fr_FR.UTF8"),
	(2060, "fr_FR.UTF8"),
	(11276, "fr_FR.UTF8"),
	(3084, "fr_FR.UTF8"),
	(9228, "fr_FR.UTF8"),
	(12300, "fr_FR.UTF8"),
	(15372, "fr_FR.UTF8"),
	(5132, "fr_FR.UTF8"),
	(13324, "fr_FR.UTF8"),
	(6156, "fr_FR.UTF8"),
	(14348, "fr_FR.UTF8"),
	(58380, "fr_FR.UTF8"),
	(8204, "fr_FR.UTF8"),
	(10252, "fr_FR.UTF8"),
	(4108, "fr_FR.UTF8"),
	(7180, "fr_FR.UTF8"),
	(1060, "sl_SI.UTF8")]

-- http://msdn.microsoft.com/en-us/goglobal/bb964664.aspx
foreign import stdcall unsafe "GetUserDefaultUILanguage"
    getUserDefaultUILanguage :: IO CUShort

setEnv_ :: String -> String -> IO () 
setEnv_ key value = withCString key $ \k -> withCString value $ \v -> do 
  success <- c_SetEnvironmentVariable k v 
  unless success (throwGetLastError "setEnv") 

putEnv :: String -> IO ()
putEnv v = void (withCString v $ \vv -> c_putenv vv)
 
foreign import stdcall unsafe "windows.h SetEnvironmentVariableA" 
  c_SetEnvironmentVariable :: CString -> CString -> IO Bool 

foreign import ccall unsafe "putenv" c_putenv :: CString -> IO CInt 

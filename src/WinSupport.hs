-- this module is only used on windows.
module WinSupport where

import Control.Monad (liftM)
import System.FilePath.Windows (splitFileName)
import System.Environment (getExecutablePath)

getDataFileName :: String -> IO String
getDataFileName fname = do
	appFolder <- liftM (fst . splitFileName) getExecutablePath
	return $ appFolder ++ fname

module SearchCommand.FindExecutable where

-- External imports
import Control.Monad
import Data.Maybe
import System.Directory

-- Local imports
import Env
import System.Err

findExecutableSearch :: ConfigSearchCmd
findExecutableSearch env fp prg = do
    prgfp <- findExecutable prg
    let notOrphan = isJust prgfp
    when (notOrphan && showNotOrphan (appArgs env)) $
      putErrLn $ prg ++ " might be responsible for " ++ fp
    return $ Right notOrphan

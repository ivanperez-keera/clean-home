-- | This program checks every file/dir in $HOME against
-- a list of know programs/packages to determine whether
-- we have any program installed that may need it.

import           Control.Monad.IfElse
import           Data.List
import qualified Data.Map               as M
import           System.Directory
import           System.Console.CmdArgs

-- Local imports
import Args
import Env
import SearchCommands
import System.Err

main :: IO ()
main = do
  -- Process args and build an env
  env <- buildEnv =<< cmdArgs chArgs
  awhen env $ \env' ->
    -- Get config files and check them
    mapM_ (checkFile env') =<< getHomeConfFiles env'

-- | Gets the list of filepaths that may be config paths
getHomeConfFiles :: Env -> IO [FilePath]
getHomeConfFiles _env = do
  homeDir <- getHomeDirectory  
  names   <- getDirectoryContents homeDir
  return $ filter isConfigPath $ sort names
  where isConfigPath :: String -> Bool
        isConfigPath x = x `notElem` [".", ".."] && "." `isPrefixOf` x

-- Checks a file using all the know programs and packages
-- that may be responsible for it
checkFile :: Env -> FilePath -> IO ()
checkFile env fp = checkFile' env fp allChks
   where allChks  = prgChks' ++ pkgChks'
         pkgChks' = if byPackage (appArgs env) then pkgChks else []
         prgChks' = if byProgram (appArgs env) then prgChks else []
         pkgChks  = map (checkPackage env fp) pkgs
         prgChks  = map (checkProgram env fp) prgs
         pkgs     = packageSearch env fp
         prgs     = programSearch env fp

-- | Prints a message indicating that the file is orphan (when it is)
-- and shows error messages when appropriate
checkFile' :: Env -> FilePath -> [IO (Either String Bool)] -> IO ()
checkFile' _env fp []     = putErrLn $ "Warning: Orphan dir: " ++ fp
checkFile' env  fp (x:xs) = do
  res <- x
  case res of
   (Left s)      -> putErrLn s
   (Right False) -> checkFile' env fp xs
   (Right True)  -> return ()

-- | Search config cp and return associated packages
packageSearch :: Env -> String -> [ String ]
packageSearch env k = M.findWithDefault [] k (M.fromList pkgs)
  where pkgs = pkgList env

-- | Search config cp and return associated programs
programSearch :: Env -> String -> [ String ]
programSearch env k = M.findWithDefault [] k (M.fromList prgs)
  where prgs = prgList env

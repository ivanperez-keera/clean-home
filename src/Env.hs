module Env
   ( module Env
   , module Args
   )
  where

import Control.Monad
import Control.Exception as E
import Control.Exception.Extra

import Args
import System.Err
import Paths_clean_home

data Env = Env
 { appArgs :: Args
 , pkgList :: PkgList
 , prgList :: PrgList
 }

type PrgList = [ (String, [ String ]) ]
type PkgList = [ (String, [ String ]) ]

type ConfigSearchCmd = Env -> FilePath -> String -> IO (Either String Bool)

-- | Reads and returns the program list contained in
-- the file PrgList.
readPrgList :: Args -> IO (Maybe PrgList)
readPrgList appArgs
 | byProgram appArgs = readListFP "data/PrgList" "program list"
 | otherwise         = return $ Just []

-- | Reads and returns the package list contained in
-- the file PrgList.
readPkgList :: Args -> IO (Maybe PkgList)
readPkgList appArgs
 | byPackage appArgs = readListFP "data/PkgList" "package list"
 | otherwise         = return $ Just []

readListFP :: Read a => FilePath -> String -> IO (Maybe [a])
readListFP fn n = do
  fp <- getDataFileName fn
  list <- E.handle (anyway (return [])) $
            liftM reads $ readFile fp
  case list of
   []        -> do putErrLn $ "Cannot read or parse " ++ n
                   return Nothing
   ((x,_):_) -> return (Just x)

-- FIXME: MaybeT?
buildEnv :: Args -> IO (Maybe Env)
buildEnv appArgs 
  | not (pkg || prg)
  = do putErrLn "You must enable --by-package or --by-program"
       return Nothing
  | otherwise 
  = do pkgList <- readPkgList appArgs
       prgList <- readPrgList appArgs
       case (pkgList, prgList) of
         (Just pkgs, Just prgs) -> return $ Just $ Env appArgs pkgs prgs
         _                      -> return Nothing
  where pkg = byPackage appArgs
        prg = byProgram appArgs

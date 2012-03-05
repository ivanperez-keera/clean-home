{-# LANGUAGE DeriveDataTypeable #-}
module Args where

import Data.Data
import System.Console.CmdArgs

-- This is the CLI app definition : what we get from the user
data Args = Args {
  showNotOrphan   :: Bool
  , byPackage     :: Bool
  , byProgram     :: Bool
  , pkgSearchProg :: String
  , pkgSearchArgs :: [String]
  , pkgSearchExitCode :: Bool
  
  -- To be added
  --   home          :: String   -- Use this dir instead of the current
  --                             -- user's home dir
  -- , verbose       :: Bool     -- Show everything you do
  -- , ignore        :: [String] -- Ignore these filepaths
  -- , remove        :: Bool     -- Give users the option to remove orphan fps
  }
 deriving (Show, Data, Typeable)

-- This is the cmdArgs-based CLI interface definition
chArgs :: Args
chArgs = Args
 {  showNotOrphan = False
 &= explicit
 &= name "show-not-orphan"
 &= help "Whether dirs with an owning program show be shown" 
 
 , byPackage = enum [ True  &= explicit &= name "by-package"
                            &= help "Search by package name (enabled by default)"
                    , False &= explicit &= name "not-by-package"
                            &= help "Do not search by package name"
                    ]
 -- &= explicit
 -- &= name "by-package"
 -- &= help "Search the responsible program by package name"

 , byProgram = enum [ True  &= explicit &= name "by-program"
                            &= help "Search by program name (enabled by default)"
                    , False &= explicit &= name "not-by-program"
                            &= help "Do not search by program name"
                    ]

 , pkgSearchProg = def
 &= explicit
 &= name "with-pkg-search"
 &= help "Program used to search for packages"
 &= typ "PROGRAM"

 , pkgSearchArgs = def
 &= explicit
 &= name "with-pkg-search-opt"
 &= help "Arguments passed to pkg-search program"
 &= typ "ARG"

 , pkgSearchExitCode = False
 &= explicit
 &= name "with-pkg-search-exit-code"
 &= help "Use program exit code to determine package presence"
 
 }
 &= summary "clean-home 0.0.1"
 &= details [ "(c) 2012 Ivan Perez - Keera Studios"
            , "Find more about clean-home at http://keera.es"
            , "and http://github.com/ivanperez-keera/clean-home"
            ]

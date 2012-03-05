module SearchCommand.UserSet
   ( userSetSearch
   , userSetSearchExitCode
   , userSetSearchOutput
   , userSetSearchCustom
   , userSetSearchCombined
   )
  where

-- External imports
import Control.Monad
import Control.Exception as E
import Control.Exception.Extra
import HSH

-- Local imports
import Args
import Env
import System.Err

userSetSearch :: ConfigSearchCmd
userSetSearch env fp pkg
  | pkgSearchExitCode (appArgs env) = userSetSearchExitCode cmd env fp pkg
  | otherwise                       = userSetSearchOutput   cmd env fp pkg
  where cmd = (pkgSearchProg (appArgs env),
               pkgSearchArgs (appArgs env) ++ [ pkg ]
              )

type CMD = (String, [String])

userSetSearchExitCode :: CMD -> ConfigSearchCmd
userSetSearchExitCode = userSetSearchCustom (Right False) (const True)

userSetSearchOutput :: CMD -> ConfigSearchCmd
userSetSearchOutput cmd env fp pkg =
  userSetSearchCustom (Left (pkg ++ " exited with error code"))
                      (not.null)
                      cmd env fp pkg

userSetSearchCombined :: CMD -> ConfigSearchCmd
userSetSearchCombined = userSetSearchCustom (Right False) (not.null)
     
userSetSearchCustom :: Either String Bool
                    -> ([String] -> Bool)
                    -> CMD
                    -> ConfigSearchCmd
userSetSearchCustom exceptRet test cmd env fp pkg =
   E.handle (anyway (return exceptRet)) $ do
    results <- run cmd :: IO [String]
    let notOrphan = test results
    when (notOrphan && showNotOrphan (appArgs env)) $
      putErrLn $ pkg ++ " might be responsible for " ++ fp
    return $ Right notOrphan


module SearchCommands where

-- Local imports
import Args
import Env
import SearchCommand.Apt
import SearchCommand.Which
import SearchCommand.UserSet

-- FIXME: The problem is here
checkPackage :: ConfigSearchCmd
checkPackage env fp pkg
  | null (pkgSearchProg (appArgs env)) = aptSearch env fp pkg
  | otherwise                          = userSetSearch env fp pkg

checkProgram :: ConfigSearchCmd
checkProgram = whichSearch

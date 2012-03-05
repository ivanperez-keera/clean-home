module SearchCommand.Apt where

import Data.List

-- Local imports
import Env
import SearchCommand.UserSet

aptSearch :: ConfigSearchCmd
aptSearch env fp pkg = userSetSearchCustom
  (Left "apt-get exited with error code")
  (\results -> not (null results) && any ((pkg ++ " -") `isPrefixOf`) results)
  cmd env fp pkg
  where cmd = ("apt-cache", [ "search"
                            , "--installed"
                            , "--names-only"
                            , "^" ++ pkg ++ "$"]
              )

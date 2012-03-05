module SearchCommand.Which where

-- Local imports
import Env
import SearchCommand.UserSet

whichSearch :: ConfigSearchCmd
whichSearch env fp prg = userSetSearchCombined cmd env fp prg
  where cmd = ("which", [ prg ])

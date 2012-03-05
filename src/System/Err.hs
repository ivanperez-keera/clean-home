module System.Err where

import System.IO

putErr :: String -> IO()
putErr = hPutStr stderr

putErrLn :: String -> IO()
putErrLn = hPutStrLn stderr

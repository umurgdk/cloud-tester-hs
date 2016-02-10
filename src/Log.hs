module Log
   ( logIO
   ) where

import Control.Monad.IO.Class

import Types

logIO :: String -> ExceptIO ()
logIO = liftIO . putStrLn



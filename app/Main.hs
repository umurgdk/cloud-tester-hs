module Main where

import Control.Monad

import Types
import Log
import DockerMachine
import DockerMachine.Types

defaultMachineName = "cloud-tester-hs"

main :: IO ()
main = void $ runExceptIOT $ do
    logIO $ "Looking for docker machine " ++ defaultMachineName ++ "..."

    dockerMachine <- getOrCreateMachine defaultMachineName

    logIO $ "Using docker machine " ++ machineName dockerMachine ++ "."




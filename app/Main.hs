{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe

import           DockerMachine
import           DockerMachine.Types
import           Log
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Types

import qualified API.Environment          as AE

defaultMachineName = "cloud-tester-hs"

myApp :: Network.Wai.Application
myApp = serve AE.environmentAPI AE.server

main :: IO ()
main = void $ runExceptIOT $ do
    logIO $ "Looking for docker machine " ++ defaultMachineName ++ "..."

    dockerMachine <- getOrCreateMachine defaultMachineName

    case machineState dockerMachine of
        Stopped -> throwE "Docker machine is not runinng..."
        Running -> void $ return ()

    logIO $ "Using docker machine " ++ machineName dockerMachine ++ " @ " ++ fromJust (machineUrl dockerMachine)

    void $ liftIO $ run 8080 myApp

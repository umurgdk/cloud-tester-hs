{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.Maybe

import           DockerMachine
import           DockerMachine.Types
import           Log
import           Types

defaultMachineName = "cloud-tester-hs"

main :: IO ()
main = void $ runExceptIOT $ do
    logIO $ "Looking for docker machine " ++ defaultMachineName ++ "..."

    dockerMachine <- getOrCreateMachine defaultMachineName

    case machineState dockerMachine of
        Stopped -> throwE "Docker machine is not runinng..."
        Running -> void $ return ()

    logIO $ "Using docker machine " ++ machineName dockerMachine ++ " @ " ++ fromJust (machineUrl dockerMachine)

    logIO "Creating default Chrome Environment..."



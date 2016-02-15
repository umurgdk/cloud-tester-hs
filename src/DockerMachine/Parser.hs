{-# LANGUAGE OverloadedStrings #-}

module DockerMachine.Parser
   ( parseLine
   , parseList
   , validateCreateMachine ) where

import           Data.List           (elem)
import           Data.Semigroup
import qualified Data.Text           as T

import           DockerMachine.Types

parseMachineState :: T.Text -> MachineState
parseMachineState "Running" = Running
parseMachineState _ = Stopped

parseLine :: T.Text -> DockerMachine
parseLine input = DockerMachine (T.unpack name) state (T.unpack <$> url)
    where
        words'   = T.words input
        name     = head words'
        state    = parseMachineState $ words' !! 3
        parseUrl = T.replace "tcp://" "http://"
        url      = (<> "/") <$> case words' !! 4 of
                       "Unknown" -> Nothing
                       address   -> Just $ parseUrl address


parseList :: T.Text -> [DockerMachine]
parseList input = fmap parseLine $ tail $ T.lines input

validateCreateMachine :: String -> T.Text -> Bool
validateCreateMachine name output = and $ fmap ($ outputLines) checks
    where
        outputLines = T.lines output
        checks =
            [ elem "Docker is up and running!"
            , elem $ "To see how to connect Docker to this machine, run: docker-machine env " <> T.pack name ]

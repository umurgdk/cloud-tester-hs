module DockerMachine.Parser
   ( parseLine
   , parseList
   , validateCreateMachine ) where

import Data.List (elem)
import DockerMachine.Types

parseLine :: String -> DockerMachine
parseLine input = DockerMachine name state url
    where
        words' = words input
        name   = head words'
        state  = read $ words' !! 3
        url    = case words' !! 4 of
                    "Unknown" -> Nothing
                    address -> Just address


parseList :: String -> [DockerMachine]
parseList input = fmap parseLine $ tail $ lines input

validateCreateMachine :: String -> String -> Bool
validateCreateMachine name output = and $ fmap ($ outputLines) checks
    where
        outputLines = lines output
        checks = 
            [ elem "Docker is up and running!"
            , elem $ "To see how to connect Docker to this machine, run: docker-machine env " ++ name ]

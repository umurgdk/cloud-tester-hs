module DockerMachine.Types where

import Data.List (find)

data MachineState = Running
                  | Stopped
                  deriving (Show, Eq, Read)

data DockerMachine = DockerMachine
    { machineName  :: String
    , machineState :: MachineState
    , machineUrl   :: Maybe String
    } deriving (Show, Eq)

findMachineByName :: String -> [DockerMachine] -> Maybe DockerMachine
findMachineByName query = find ((== query) . machineName)


module DockerMachine
  ( getMachines
  , createMachine
  , getOrCreateMachine
  , findMachineByName
  ) where

import Control.Monad
import Control.Monad.Trans.Class

import Data.Either
import Data.Maybe
import Data.ByteString.Char8 as BC
import qualified Control.Monad.Trans.Except as E

import Types
import SafeProcess

import DockerMachine.Types
import DockerMachine.Parser

getMachines :: ExceptIO [DockerMachine]
getMachines = liftM parseList result
    where 
        result = run "docker-machine ls"

createMachine :: String -> ExceptIO String
createMachine name =
    run $ "docker-machine create --driver \"virtualbox\" " ++ name

-- | Try to find docker machine and if not exists
-- | try to create a new one
getOrCreateMachine :: String -> ExceptIO DockerMachine
getOrCreateMachine name =
    findMachine <$> getMachines >>= maybe create return
    where 
        create = createMachine name >> getOrCreateMachine name
        findMachine = findMachineByName name

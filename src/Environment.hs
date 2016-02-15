{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Environment where

import           Data.Maybe
import           Data.Semigroup
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Control.Monad.Trans.Except as E

import           DockerMachine
import           DockerMachine.Types
import           Types

data Version = Latest
             | Version Int
             deriving (Show, Eq)

data Browser = Chrome Version
             | Firefox Version
             deriving (Show, Eq)

data Platform = Linux
              | MacOSX
              | Windows
              deriving (Show, Eq)

data LiveNode = LiveNode
    { nodeId          :: String
    , nodeIP          :: String
    , nodeDescription :: NodeDescription
    } deriving (Show, Eq)

data NodeDescription = NodeDescription
    { nodeName        :: String
    , nodeBrowser     :: Browser
    , nodePlatform    :: Platform
    , nodeDockerImage :: String
    } deriving (Show, Eq)


defaultChromeDescription :: NodeDescription
defaultChromeDescription = NodeDescription
    { nodeName = "Default Chrome"
    , nodeBrowser = Chrome Latest
    , nodePlatform = Linux
    , nodeDockerImage = "selenium/standalone-chrome"
    }

defaultFirefoxDescription :: NodeDescription
defaultFirefoxDescription = NodeDescription
    { nodeName = "Default Firefox"
    , nodeBrowser = Firefox Latest
    , nodePlatform = Linux
    , nodeDockerImage = "selenium/standalone-firefox"
    }

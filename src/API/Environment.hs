{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module API.Environment where

import           Data.Text
import           Servant
{-import           Servant.API-}

type EnvironmentAPI = "environments" :> Get '[JSON] [Environment]
                 {-:<|> "environment"  :> Capture "environmentId" Int :> Get '[JSON] Environment-}

data Environment = Environment
    { environmentId   :: Int
    , environmentName :: String
    }

sampleEnvironment = Environment 0 "Home"

environments :: [Environment]
environments = [ sampleEnvironment ]

server :: Server EnvironmentAPI
server = return environments

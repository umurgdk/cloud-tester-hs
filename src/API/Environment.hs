{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module API.Environment where

import           Data.Aeson
import           Data.Text    as T
import           GHC.Generics
import           Servant

type EnvironmentAPI = "environments" :> Get '[JSON] [Environment]
                 {-:<|> "environment"  :> Capture "environmentId" Int :> Get '[JSON] Environment-}

data Environment = Environment
    { environmentId   :: Int
    , environmentName :: !T.Text
    } deriving Generic

instance ToJSON Environment where
    toJSON = genericToJSON defaultOptions

sampleEnvironment = Environment 0 "Home"

environments :: [Environment]
environments = [ sampleEnvironment ]

server :: Server EnvironmentAPI
server = return environments

environmentAPI :: Proxy EnvironmentAPI
environmentAPI = Proxy

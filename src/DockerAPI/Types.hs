{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module DockerApi.Types where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens  (key, _String)
import           Data.Aeson.Types
import           Data.Char
import qualified Data.Text        as T
import           GHC.Generics

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs
capitalize str = str

dockerName :: String -> String
dockerName name = capitalize $ dropWhile (not . isUpper) name

data DockerInfo = DockerInfo
    { dockerInfoName       :: !T.Text
    , dockerInfoContainers :: Int
    , dockerInfoImages     :: Int
    } deriving (Show, Generic)

makeFields ''DockerInfo
instance FromJSON DockerInfo

data Container = Container
    { containerId      :: !T.Text
    , containerNames   :: ![T.Text]
    , containerImage   :: !T.Text
    , containerCommand :: !T.Text
    , containerStatus  :: !T.Text
    } deriving (Show, Generic)

makeFields ''Container
instance FromJSON Container

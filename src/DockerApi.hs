{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module DockerApi where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy.Char8  as B
import           Data.Char
import qualified Data.Text                   as T
import           GHC.Generics

import           Network.HTTP.Client.OpenSSL
import           Network.Wreq

import           OpenSSL                     (withOpenSSL)
import           OpenSSL.Session             (SSLContext)
import qualified OpenSSL.Session             as SSL

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

instance FromJSON DockerInfo where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = dockerName }


data Container = Container
    { containerId      :: !T.Text
    , containerNames   :: ![T.Text]
    , containerImage   :: !T.Text
    , containerCommand :: !T.Text
    , containerStatus  :: !T.Text
    } deriving (Show, Generic)

makeFields ''Container

instance FromJSON Container where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = dockerName }

getSSLContext :: IO SSLContext
getSSLContext = do
    ctx <- SSL.context
    SSL.contextSetPrivateKeyFile   ctx "/Users/umurgdk/.docker/machine/machines/cloud-tester-hs/key.pem"
    SSL.contextSetCertificateFile  ctx "/Users/umurgdk/.docker/machine/machines/cloud-tester-hs/cert.pem"
    SSL.contextAddOption           ctx SSL.SSL_OP_NO_SSLv2
    SSL.contextAddOption           ctx SSL.SSL_OP_NO_SSLv3
    return ctx

optionsWithSSL = defaults & manager .~ Left (opensslManagerSettings getSSLContext)

buildRestURL :: String -> String -> String
buildRestURL host path = "https://" ++ host ++ ":2376" ++ path

getJSON :: FromJSON a => String -> IO a
getJSON url = withOpenSSL $ (^. responseBody) <$> (asJSON =<< getWith optionsWithSSL url)

getText url = withOpenSSL $ (^. responseBody) <$> getWith optionsWithSSL url

getDockerInfo :: String -> IO DockerInfo
getDockerInfo host = getJSON $ buildRestURL host "/info"

getContainers :: String -> IO [Container]
getContainers host = getJSON $ buildRestURL host "/containers/json?all=1"


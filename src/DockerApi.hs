{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module DockerApi where

import           Control.Lens                hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Lens             (key, _String)
import           Data.Aeson.Types
import           Data.ByteString.Lazy        (ByteString, toStrict)
import qualified Data.ByteString.Lazy.Char8  as B
import           Data.Char
import           Data.List                   (find)
import           Data.Maybe
import qualified Data.Text                   as T
import           Data.Text.Encoding
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

getText :: String -> IO T.Text
getText url = withOpenSSL $ responseBodyAsText <$> getWith optionsWithSSL url
    where responseBodyAsText = decodeUtf8 . toStrict . (^. responseBody)

postJSON :: (FromJSON a, ToJSON b) => String -> b -> IO a
postJSON url postData = withOpenSSL $ (^. responseBody) <$> (asJSON =<< postWith optionsWithSSL url (toJSON postData))

postJSON_ :: ToJSON a => String -> a -> IO T.Text
postJSON_ url postData = withOpenSSL $ responseBodyAsText <$> postWith optionsWithSSL url (toJSON postData)
    where responseBodyAsText = decodeUtf8 . toStrict . (^. responseBody)

getDockerInfo :: String -> IO DockerInfo
getDockerInfo host = getJSON $ buildRestURL host "/info"

getContainers :: String -> IO [Container]
getContainers host = getJSON $ buildRestURL host "/containers/json?all=1"

getContainer :: String -> String -> IO Container
getContainer host id = getJSON $ buildRestURL host $ "/containers/" ++ id ++ "/json"

createContainer :: String ->  String -> IO Container
createContainer host imageName = do
    response <- postJSON url createData
    let (Just id) = parseMaybe (.: "Id") response :: Maybe T.Text

    fromJust . find (finder id) <$> getContainers host

    where url = buildRestURL host "/containers/create"
          createData = object [ "Image" .= imageName ]
          finder id' container = id' == (container ^. DockerApi.id)




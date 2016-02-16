{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module DockerAPI where

import           Control.Lens                hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Lens             (key, _String)
import           Data.Aeson.Types
import           Data.ByteString.Lazy        (ByteString, toStrict)
import qualified Data.ByteString.Lazy.Char8  as B
import           Data.List                   (find)
import           Data.Maybe
import qualified Data.Text                   as T
import           Data.Text.Encoding

import           Network.HTTP.Client.OpenSSL
import           Network.Wreq

import           OpenSSL                     (withOpenSSL)
import           OpenSSL.Session             (SSLContext)
import qualified OpenSSL.Session             as SSL

import qualified DockerAPI.Types             as DT

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

getDockerInfo :: String -> IO DT.DockerInfo
getDockerInfo host = getJSON $ buildRestURL host "/info"

getContainers :: String -> IO [DT.Container]
getContainers host = getJSON $ buildRestURL host "/containers/json?all=1"

getContainer :: String -> String -> IO DT.Container
getContainer host id = getJSON $ buildRestURL host $ "/containers/" ++ id ++ "/json"

createContainer :: String ->  String -> IO DT.Container
createContainer host imageName = do
    response <- postJSON url createData
    let (Just id) = parseMaybe (.: "Id") response :: Maybe T.Text

    fromJust . find (finder id) <$> getContainers host

    where url = buildRestURL host "/containers/create"
          createData = object [ "Image" .= imageName ]
          finder id' container = id' == (container ^. DT.id)




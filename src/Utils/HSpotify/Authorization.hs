module Utils.HSpotify.Authorization where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad

import qualified Data.ByteString as BS
import Data.Text (Text, pack, unpack, unlines)
import Data.Text.Encoding
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Header
import System.Environment
import System.Exit
import System.Random
import System.Process


import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp
  ( runSettings,
    setPort,
    setHost,
    setBeforeMainLoop,
    defaultSettings,
    Settings (..),
  )

redirectUri :: Text
redirectUri = "http://localhost:7979"

oauthAuthorizeUrl :: Text
oauthAuthorizeUrl = "https://accounts.spotify.com/authorize"

oauthTokenUrl :: Text
oauthTokenUrl = "https://accounts.spotify.com/token"


pageHTML =
  "<!DOCTYPE HTML>\
  \<html>\
  \<head></head>\
  \<body>\
  \<span id=\"display\">oldtext</span>\
  \<script>\
  \var hash=window.location.hash.substring(1);\
  \var next=\"http://localhost:7979/?\"+hash;\
  \document.getElementById(\"display\").textContent=\"You can now close this window.\";\
  \if (hash.length != 0) {\
  \   window.location.replace(next);\
  \} else {\
  \   window.close();\
  \}\
  \</script>\
  \<body>\
  \</html>"

authUrl :: Text -> Text -> Text -> Text
authUrl clientId scope state =
  oauthAuthorizeUrl
    <> "?response_type=token"
    <> "&client_id=" <> clientId
    <> "&scope=" <> scope
    <> "&redirect_uri=" <> redirectUri
    <> "&state=" <> state


printPathInfo :: Network.Wai.Request -> IO ()
printPathInfo req = do
  let query = Network.Wai.queryString req :: [(BS.ByteString, Maybe BS.ByteString)]
      tokenParam = join $ lookup "access_token" query :: Maybe BS.ByteString
      stateParam = join $ lookup "state" query :: Maybe BS.ByteString
   in do
      print tokenParam
      print stateParam

getToken :: Network.Wai.Request -> IO Text
getToken req = do
  let query = Network.Wai.queryString req :: [(BS.ByteString, Maybe BS.ByteString)]
      tokenParam = join $ lookup "access_token" query :: Maybe BS.ByteString
   in case tokenParam of
        Just t -> return $ decodeUtf8 t
        _ -> return ""

respondOk :: Application
respondOk _ respond =
  respond $ responseLBS status200 [(hContentType, "text/html")] pageHTML

respondOk' :: MVar Text -> Application
respondOk' token req respond = do
  modifyMVar token $ \_ -> do
    t' <- getToken req
    resp <- respondOk req respond
    return (t', resp)


initToken :: IO Text
initToken = do
  token <- newEmptyMVar
  putMVar token (pack "")
  withAsyncBound ( listenRedirect settings token ) asyncMakeRequest
  readMVar token

  where
    settings = ( setPort 7979 . setHost "127.0.0.1" ) defaultSettings


listenRedirect :: Settings -> MVar Text -> IO ()
listenRedirect settings token = do
  runSettings settings $ respondOk' token

asyncMakeRequest :: Async a -> IO ()
asyncMakeRequest _ = makeRequest

makeRequest :: IO ()
makeRequest = do
  client_id <- getClientID

  let scope = "user-read-private user-read-email"

  code <- openBrowser (authUrl client_id scope "0123456789ABCDEF")
  threadDelay 1000000

openBrowser :: Text -> IO ExitCode
openBrowser url = rawSystem "xdg-open" [unpack url]


getClientID :: IO Text
getClientID = do
  client_id <- lookupEnv "SPOTIFY_CLIENT_ID"
  case client_id of
    Just cid -> return $ pack cid
    Nothing -> exitFailure

module Utils.HSpotify.Internal
  ( Uri (..),

    Token,

    apiV1,
    getApi,
    postApi,
    putApi
  )
where

import Data.Aeson

import Data.Text
import Data.Text.Encoding

import Network.HTTP.Client
import Network.HTTP.Client.TLS
--import Network.HTTP.Types.Status (statusCode)

import Network.HTTP.Simple


class Uri a where
  uri :: a -> Text


type Token = Text


apiV1 :: Text
apiV1 = "https://api.spotify.com/v1/"


getApi :: (FromJSON a) => Token -> Text -> IO (Maybe a)
getApi token endpoint = do
  manager <- newManager tlsManagerSettings

  request' <- parseRequest $ unpack $ apiV1 <> endpoint
  let request =
        setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 token]
        $ setRequestHeader "Content-Type" ["application/json"]
        request'
  response <- Network.HTTP.Client.httpLbs request manager

--  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
--  print $ responseBody response
  return $ decode $ responseBody response


putApi :: (ToJSON a) => Token -> Text -> a -> IO ()
putApi token endpoint body = do
  manager <- newManager tlsManagerSettings

  request' <- parseRequest $ unpack $ apiV1 <> endpoint
  let request =
        setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 token]
        $ setRequestHeader "Content-Type" ["application/json"]
        request'
          { method = "PUT",
            requestBody = RequestBodyLBS $ encode body
          }
  _ <- Network.HTTP.Client.httpLbs request manager
  return ()


postApi :: (ToJSON a) => Token -> Text -> a -> IO ()
postApi token endpoint body = do
  manager <- newManager tlsManagerSettings

  request' <- parseRequest $ unpack $ apiV1 <> endpoint
  let request =
        setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 token]
        $ setRequestHeader "Content-Type" ["application/json"]
        request'
          { method = "POST",
            requestBody = RequestBodyLBS $ encode body
          }
  _ <- Network.HTTP.Client.httpLbs request manager
  return ()

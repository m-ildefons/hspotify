module Utils.HSpotify.Play
  ( play
  )
where

import Data.Aeson

import Utils.HSpotify.Internal


play :: Token -> IO ()
play token = do
  _ <- putApi token "me/player/play" $ object []
  return ()

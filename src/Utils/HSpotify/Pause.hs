module Utils.HSpotify.Pause
  ( pause
  )
where

import Data.Aeson

import Utils.HSpotify.Internal


pause :: Token -> IO ()
pause token = do
  _ <- putApi token "me/player/pause" $ object []
  return ()

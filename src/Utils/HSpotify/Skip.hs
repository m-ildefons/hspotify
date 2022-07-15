module Utils.HSpotify.Skip
  ( skipToNext,
    skipToPrevious
  )
where

import Data.Aeson

import Utils.HSpotify.Internal


skipToNext :: Token -> IO ()
skipToNext token = do
  _ <- postApi token "me/player/next" $ object []
  return ()


skipToPrevious :: Token -> IO ()
skipToPrevious token = do
  _ <- postApi token "me/player/previous" $ object []
  return ()

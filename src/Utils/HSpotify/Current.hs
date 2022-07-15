module Utils.HSpotify.Current
  ( currentPlaybackState,
    currentTrack,
    recentlyPlayed
  )
where


import Data.Aeson
import Data.Coerce (coerce)
import Data.Default
import Data.Text
import GHC.Generics
import Prettyprinter

import Utils.HSpotify.Internal
import Utils.HSpotify.Track


data PlaybackState =
  PlaybackState
    { playbackIsPlaying :: Bool,
      playbackShuffle :: Bool
    }
  deriving (Generic, Show, Eq)

instance FromJSON PlaybackState where
  parseJSON =
    withObject "PlaybackState" $
      \v -> PlaybackState
              <$> v .: "is_playing"
              <*> v .: "shuffle_state"

instance Pretty PlaybackState where
  pretty state =
    nest 2
      ( vsep
          [ "Is Playing:" <+> pretty (playbackIsPlaying state)
          ]
      )


data CurrentlyPlaying =
  CurrentlyPlaying
    { currentItem :: Track,
      currentProgress_ms :: Int
    }
  deriving (Generic, Show, Eq)

instance FromJSON CurrentlyPlaying where
  parseJSON =
    withObject "CurrentlyPlaying" $
      \v -> CurrentlyPlaying
              <$> v .: "item"
              <*> v .: "progress_ms"

instance Pretty CurrentlyPlaying where
  pretty playing =
    nest 2
      ( vsep
          [ "Currently Playing:",
            pretty ( currentItem playing ),
            "progress (%):" <+> pretty ( progressPct playing )
          ]
      )


data RecentlyPlayed =
  RecentlyPlayed
    { recentItems :: [Playable],
      recentItemNumber :: Int
    }
  deriving (Generic, Show, Eq)

data Playable = Playable Track
  deriving (Generic, Show, Eq)

instance FromJSON Playable where
  parseJSON =
    withObject "Playable" $
      \v -> Playable <$> ( v .: "track" )

instance Pretty Playable where
  pretty (Playable track) = pretty track

instance FromJSON RecentlyPlayed where
  parseJSON =
    withObject "RecentlyPlayed" $
      \v -> do
        items <- v .: "items"
        total <- v .: "limit"
        let recentItems = coerce ( items :: [Playable] )
            recentItemNumber = total
        return RecentlyPlayed {..}

instance Pretty RecentlyPlayed where
  pretty played =
    nest 2
      ( vsep
          [ pretty ( recentItemNumber played ) <+> "most recently played Tracks:",
            pretty ( recentItems played )
          ]
      )


progressPct :: CurrentlyPlaying -> Double
progressPct playing =
  fromIntegral ( currentProgress_ms playing )
    / fromIntegral ( trackDuration_ms ( currentItem playing ) )
    * 100

currentPlaybackState :: Token -> IO (Maybe PlaybackState)
currentPlaybackState token = getApi token "me/player"

currentTrack :: Token -> IO (Maybe CurrentlyPlaying)
currentTrack token = getApi token "me/player/currently-playing"

recentlyPlayed :: Token -> IO (Maybe RecentlyPlayed)
recentlyPlayed token = getApi token "me/player/recently-played"

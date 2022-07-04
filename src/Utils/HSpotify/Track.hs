module Utils.HSpotify.Track
  ( Track (..),

    getTrackById
  )
where

import Data.Aeson
import Data.Text
import GHC.Generics
import Prettyprinter

import Utils.HSpotify.Internal


data Track =
  Track
    { trackId :: Text,
      trackName :: Text
    }
  deriving (Generic, Show, Eq)

instance FromJSON Track where
  parseJSON =
    withObject "Track" $
      \v -> Track
              <$> v .: "id"
              <*> v .: "name"

instance Pretty Track where
  pretty track =
    nest 2
      ( vsep
          [ "Track \"" <> pretty (trackName track) <> "\"",
            "id: " <> pretty (trackId track)
          ]
      )

-- | Retrieve a track object from a given ID
--
getTrackById :: Token -> Text -> IO (Maybe Track)
getTrackById token trackId = getApi token $ "tracks/" <> trackId

module Utils.HSpotify.Track
  ( Track (..),

    getTrackById
  )
where

import Data.Aeson
import Data.Text
import GHC.Generics
import Prettyprinter

import Utils.HSpotify.Artist
import Utils.HSpotify.Internal
import Utils.HSpotify.Utils


data Track =
  Track
    { trackId :: Text,
      trackName :: Text,
      trackDuration_ms :: Int,
      trackPopularity :: Int,
      trackArtists :: [Artist]
    }
  deriving (Generic, Show, Eq)


instance FromJSON Track where
  parseJSON =
    withObject "Track" $
      \v -> Track
              <$> v .: "id"
              <*> v .: "name"
              <*> v .: "duration_ms"
              <*> v .: "popularity"
              <*> v .: "artists"


instance Pretty Track where
  pretty track =
    nest 2
      ( vsep
          [ "Track \"" <> pretty ( trackName track ) <> "\"",
            nest 2 ( hsep ( "Artists:" : punctuate comma ( fmap an ( trackArtists track ) ) ) ),
            -- "id:" <+> pretty (trackId track),
            -- "uri:" <+> pretty (uri track),
            "duration:" <+> pretty ( pretty_ms $ trackDuration_ms track ),
            "popularity:" <+> pretty ( trackPopularity track )
          ]
      )
    where
      an artist = pretty ( artistName artist )

instance Uri Track where
  uri (Track track_id _ _ _ _) = "spotify:track:" <> track_id


-- | Retrieve a track object from a given ID
--
getTrackById :: Token -> Text -> IO (Maybe Track)
getTrackById token trackId = getApi token $ "tracks/" <> trackId

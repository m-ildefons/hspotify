module Utils.HSpotify.Artist
  ( Artist (..),

    getArtistById
  )
where


import Data.Aeson
import Data.Text
import GHC.Generics
import Prettyprinter

import Utils.HSpotify.Internal


data Artist =
  Artist
    { artistId :: Text,
      artistName :: Text,
      genres :: [Text]
    }
  deriving (Generic, Show, Eq)

instance FromJSON Artist where
  parseJSON =
    withObject "Artist" $
      \v -> Artist
              <$> v .: "name"
              <*> v .: "id"
              <*> v .: "genres"

instance Pretty Artist where
  pretty artist =
    nest 2
      ( vsep
          [ "Artist \"" <> pretty (artistName artist) <> "\"",
            "id: " <> pretty (artistId artist),
            "genres: " <> pretty (genres artist)
          ]
      )


-- | Retrieve the user object corresponding to the currently authenticated user.
--
getArtistById :: Token -> Text -> IO (Maybe Artist)
getArtistById token artistId = getApi token $ "artists/" <> artistId

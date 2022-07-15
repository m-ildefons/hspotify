module Utils.HSpotify.Album
  ( Album (..),

    getAlbumById
  )
where


import Data.Aeson
import Data.Text
import GHC.Generics
import Prettyprinter

import Utils.HSpotify.Internal


data Album =
  Album
    { albumId :: Text,
      albumName :: Text
    }
  deriving (Generic, Show, Eq)


instance FromJSON Album where
  parseJSON =
    withObject "Album" $
      \v -> Album
              <$> v .: "name"
              <*> v .: "id"


instance Pretty Album where
  pretty album =
    nest 2
      ( vsep
          [ "Artist \"" <> pretty (albumName album) <> "\"",
            "id: " <> pretty (albumId album)
          ]
      )


instance Uri Album where
  uri (Album album_id _) = "spotify:album:" <> album_id


-- | Retrieve the user object corresponding to the currently authenticated user.
--
getAlbumById :: Token -> Text -> IO (Maybe Album)
getAlbumById token albumId = getApi token $ "albums/" <> albumId

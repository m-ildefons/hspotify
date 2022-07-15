module Utils.HSpotify.Search
  ( SearchResult (..),

    search
  )
where


import Data.Aeson
import Data.Default
import Data.Text
import GHC.Generics
import Prettyprinter

import Utils.HSpotify.Album
import Utils.HSpotify.Internal
import Utils.HSpotify.Track


data SearchResult =
  SearchResult
    { searchResultTracks :: SearchResultTracks,
      searchResultAlbums :: SearchResultAlbums
    }
  deriving (Generic, Show, Eq)


instance FromJSON SearchResult where
  parseJSON =
    withObject "SearchResult" $
      \v -> SearchResult
              <$> v .:? "tracks" .!= def
              <*> v .:? "albums" .!= def


instance Pretty SearchResult where
  pretty searchresult =
    nest 2
      ( vsep
          [ pretty $ searchResultTracks searchresult,
            pretty $ searchResultAlbums searchresult
          ]
      )


data SearchResultTracks =
  SearchResultTracks
    { hrefTracks :: Text,
      totalTracks :: Int,
      itemsTracks :: [Track]
    }
  deriving (Generic, Show, Eq)


instance FromJSON SearchResultTracks where
  parseJSON =
    withObject "SearchResultTracks" $
      \v -> SearchResultTracks
              <$> v .: "href"
              <*> v .: "total"
              <*> v .: "items"


instance Default SearchResultTracks where
  def = SearchResultTracks "null" 0 []


instance Pretty SearchResultTracks where
  pretty searchresulttracks =
    nest 2
      ( vsep
          [ "Tracks:",
            "href: " <> pretty (hrefTracks searchresulttracks),
            "total: " <> pretty (totalTracks searchresulttracks),
            "items: " <> pretty (itemsTracks searchresulttracks)
          ]
      )


data SearchResultAlbums =
  SearchResultAlbums
    { hrefAlbums :: Text,
      totalAlbums :: Int,
      itemsAlbums :: [Album]
    }
  deriving (Generic, Show, Eq)


instance FromJSON SearchResultAlbums where
  parseJSON =
    withObject "SearchResultAlbums" $
      \v -> SearchResultAlbums
              <$> v .: "href"
              <*> v .: "total"
              <*> v .: "items"


instance Default SearchResultAlbums where
  def = SearchResultAlbums "null" 0 []


instance Pretty SearchResultAlbums where
  pretty searchresultalbums =
    nest 2
      ( vsep
          [ "Albums:",
            "href: " <> pretty (hrefAlbums searchresultalbums),
            "total: " <> pretty (totalAlbums searchresultalbums),
            "items: " <> pretty (itemsAlbums searchresultalbums)
          ]
      )


search :: Token -> Text -> IO (Maybe SearchResult)
search token query = getApi token $ "search?" <> query

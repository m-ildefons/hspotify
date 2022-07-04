module Utils.HSpotify.User
  ( User (..),

    getCurrentUser,
    getUserById
  )
where


import Data.Aeson
import Data.Text
import GHC.Generics
import Prettyprinter

import Utils.HSpotify.Internal


data User =
  User
    { country :: Text,
      displayName :: Text,
      email :: Text,
      userId :: Text
    }
  deriving (Generic, Show, Eq)

instance FromJSON User where
  parseJSON =
    withObject "User" $
      \v -> User
              <$> v .:? "country" .!= "country not accessible"
              <*> v .: "display_name"
              <*> v .:? "email" .!= "email not accessible"
              <*> v .: "id"

instance Pretty User where
  pretty user =
    nest 2
      ( vsep
          [ "User \"" <> pretty (displayName user) <> "\"",
            "id: " <> pretty (userId user)
          ]
      )

-- | Retrieve the user object corresponding to the currently authenticated user.
--
getCurrentUser :: Token -> IO (Maybe User)
getCurrentUser token = getApi token "me"

-- | Retrieve a user by id
--
getUserById :: Token -> Text -> IO (Maybe User)
getUserById token userId = getApi token $ "users/" <> userId

module Utils.HSpotify.ArtistSpec (spec) where


import Data.Aeson
import Test.Hspec

import Utils.HSpotify


spec :: SpecWith ()
spec =
  describe "Artist Api Object" $ do
    it "parse empty JSON" $ do
      let jsondata = ""
          artist = decode jsondata :: Maybe Artist
      artist `shouldBe` Nothing

    it "parse well formed JSON" $ do
      let jsondata = "{\"id\": \"foo\", \"name\": \"bar\", \"popularity\": 20, \"genres\": [\"Foo\", \"Bar\"]}"
          artist = decode jsondata :: Maybe Artist
      artist `shouldBe` Just ( Artist "foo" "bar" 20 [ "Foo", "Bar" ] )

module Utils.HSpotify.TrackSpec (spec) where


import Data.Aeson
import Test.Hspec

import Utils.HSpotify


spec :: SpecWith ()
spec =
  describe "Artist Api Object" $ do
    it "parse empty JSON" $ do
      let jsondata = ""
          track = decode jsondata :: Maybe Track
      track `shouldBe` Nothing

    it "parse well formed JSON" $ do
      let jsondata = "{\"id\": \"foo\", \"name\": \"bar\", \"duration_ms\": 100, \"popularity\": 20, \"artists\": [{\"id\": \"foo\", \"name\": \"bar\", \"popularity\": 20, \"genres\": [\"Foo\", \"Bar\"]}]}"
          track = decode jsondata :: Maybe Track
      track `shouldBe` Just ( Track "foo" "bar" 100 20 [ Artist "foo" "bar" 20 ["Foo", "Bar"] ] )

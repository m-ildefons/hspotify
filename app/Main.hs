module Main where

import Data.Text
import Prettyprinter
import Utils.HSpotify

import Options.Applicative


data Command
  = SearchCommand String String
  | UserCommand String

allCommands :: Parser Command
allCommands =
  subparser
    ( command "search" ( info searchCommand ( progDesc "Search on Spotify" ) )
      <> command "user" ( info userCommand ( progDesc "Show info on User" ) )
    )

searchCommand :: Parser Command
searchCommand =
  SearchCommand
    <$> argument str ( metavar "TYPE" )
    <*> argument str ( metavar "QUERY" )

userCommand :: Parser Command
userCommand = UserCommand <$> argument str ( metavar "USER_ID" )

execute :: Command -> IO ()
execute (SearchCommand itype query) = do
  token <- initToken
  maybe_user <- search token $ "q=" <> pack query <> "&type=" <> pack itype
  case maybe_user of
    Just u -> print $ pretty u
    Nothing -> print "failed parsing"
execute (UserCommand userId) = do
  token <- initToken
  maybe_user <- getUserById token $ pack userId
  case maybe_user of
    Just u -> print $ pretty u
    Nothing -> print "failed parsing"

main :: IO ()
main = execute =<< execParser opts
  where
    opts = info ( allCommands <**> helper )
      ( fullDesc
      <> progDesc "Spotify Command Line Interface"
      <> header "HSpotify"
      )

module Main where

import Data.Text
import Prettyprinter
import Utils.HSpotify

import Options.Applicative


data Command
  = SearchCommand String String
  | UserCommand (Maybe String)
  | PauseCommand (Maybe String)
  | PlayCommand (Maybe String)
  | NextCommand (Maybe String)
  | PreviousCommand (Maybe String)
  | PlaybackStateCommand (Maybe String)
  | CurrentTrackCommand (Maybe String)
  | PlayHistoryCommand (Maybe String)


allCommands :: Parser Command
allCommands =
  subparser
    ( command "search" ( info searchCommand ( progDesc "Search on Spotify" ) )
      <> command "user" ( info userCommand ( progDesc "Show info on User" ) )
      <> command "pause" ( info pauseCommand ( progDesc "Pause playback" ) )
      <> command "play" ( info playCommand ( progDesc "Start/Resume playback" ) )
      <> command "next" ( info nextCommand ( progDesc "Skip to next track" ) )
      <> command "prev" ( info previousCommand ( progDesc "Skip to previous track" ) )
      <> command "state" ( info playbackStateCommand ( progDesc "Show playback state" ) )
      <> command "track" ( info currentTrackCommand ( progDesc "Show current track" ) )
      <> command "history" ( info playHistoryCommand ( progDesc "Show last played tracks" ) )
    )

searchCommand :: Parser Command
searchCommand =
  SearchCommand
    <$> argument str ( metavar "TYPE" )
    <*> argument str ( metavar "QUERY" )

userCommand :: Parser Command
userCommand = UserCommand <$> optional ( argument str ( metavar "USER_ID" ) )

pauseCommand :: Parser Command
pauseCommand = PauseCommand <$> optional ( argument str ( metavar "DEVICE_ID" ) )

playCommand :: Parser Command
playCommand = PlayCommand <$> optional ( argument str ( metavar "DEVICE_ID" ) )

nextCommand :: Parser Command
nextCommand = NextCommand <$> optional ( argument str ( metavar "DEVICE_ID" ) )

previousCommand :: Parser Command
previousCommand = PreviousCommand <$> optional ( argument str ( metavar "DEVICE_ID" ) )

playbackStateCommand :: Parser Command
playbackStateCommand =
  PlaybackStateCommand
    <$> optional ( argument str ( metavar "DEVICE_ID" ) )

currentTrackCommand :: Parser Command
currentTrackCommand =
  CurrentTrackCommand
    <$> optional ( argument str ( metavar "DEVICE_ID" ) )

playHistoryCommand :: Parser Command
playHistoryCommand =
  PlayHistoryCommand
    <$> optional ( argument str ( metavar "DEVICE_ID" ) )


execute :: Command -> IO ()
execute (SearchCommand itype query) = do
  token <- initToken
  maybe_user <- search token $ "q=" <> pack query <> "&type=" <> pack itype
  case maybe_user of
    Just u -> print $ pretty u
    Nothing -> print "failed parsing"
execute (UserCommand userId) = do
  token <- initToken
  maybe_user <-
    case userId of
      Just uid -> getUserById token $ pack uid
      Nothing -> getCurrentUser token
  case maybe_user of
    Just u -> print $ pretty u
    Nothing -> print "failed parsing user"
execute (PauseCommand _) = do
  token <- initToken
  pause token
execute (PlayCommand _) = do
  token <- initToken
  play token
execute (NextCommand _) = do
  token <- initToken
  skipToNext token
execute (PreviousCommand _) = do
  token <- initToken
  skipToPrevious token
execute (PlaybackStateCommand _) = do
  token <- initToken
  maybe_state <- currentPlaybackState token
  case maybe_state of
    Just s -> print $ pretty s
    Nothing -> print "failed parsing playback state"
execute (CurrentTrackCommand _) = do
  token <- initToken
  maybe_track <- currentTrack token
  case maybe_track of
    Just t -> print $ pretty t
    Nothing -> print "failed parsing current track"
execute (PlayHistoryCommand _) = do
  token <- initToken
  maybe_tracks <- recentlyPlayed token
  case maybe_tracks of
    Just t -> print $ pretty t
    Nothing -> print "failed parsing recent tracks"

main :: IO ()
main = execute =<< execParser opts
  where
    opts = info ( allCommands <**> helper )
      ( fullDesc
          <> progDesc "Spotify Command Line Interface"
          <> header "HSpotify"
      )

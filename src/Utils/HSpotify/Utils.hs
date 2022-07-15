module Utils.HSpotify.Utils
  ( pretty_ms
  )
where


pretty_ms :: Int -> String
pretty_ms t =
  show ( minutes t )
    <> " minutes "
    <> show ( seconds t )
    <> " seconds"
  where
    minutes ms = div (div ms 1000) 60
    seconds ms = mod (div ms 1000) 60

# hspotify

2022 SUSE Hackweek project.

This is a POC implementation of parts of the Spotify API as Haskell bindings,
including a command line client.
The state of this project is whatever the result of a couple of days worth of
hacking were during SUSE Hackweek 2022.

To use or develop this project, you need to register an application for the
Spotify API with the Spotify Developer Portal. There you will obtain a client
ID, which needs to be exported into your shells environment

```bash
export SPOTIFY_CLEINT_ID=foobar
```

Once you are logged into your Spotify account with your browser, you can use the
API or the CLI client.

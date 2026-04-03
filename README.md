# disco-rip

Haskell package for adding Discord Rich Presence to applications via local IPC Protocol.

## Installation

Add `disco-rip` to your `package.yaml` or `.cabal` file dependencies. It requires the `network`, `aeson`, `stm`, and `async` packages to operate efficiently.

## Usage

Here is a basic example of how to start the client, update the rich presence activity, and close the client.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import DiscoRip.Client
import DiscoRip.Command

main :: IO ()
main = do
  -- 1. Start the client with your Application ID (Client ID) and a tracing flag
  -- This will automatically look for the IPC socket and manage connection retries in the background.
  handle <- startClient (ClientConfig "YOUR_DISCORD_CLIENT_ID" False) print
  waitReady handle

  -- 2. Define the activity you want to display
  -- Use mkActivity to quickly create an Activity with just state and optional details
  let
    baseActivity = mkActivity "Exploring the Codebase" (Just "Writing Haskell")
    myActivity = baseActivity
      { timestamps = Just (ActivityTimestamps (Just 1600000000) Nothing)
      , assets = Just (ActivityAssets (Just "haskell_logo") (Just "Haskell") Nothing Nothing)
      , instance_ = Just False
      , buttons = Just [ActivityButton "View Source" "https://github.com/dpwiz/disco-rip"]
      }

  -- 3. Send the command to Discord
  -- The `setActivity` function will automatically fetch your process ID
  -- and package the request. The call function blocks until Discord
  -- responds to the command.
  response <- setActivity handle myActivity
  print response

  -- Alternatively, listen for any raw events coming from Discord
  -- event <- atomically $ readTBQueue (eventsQueue handle)
  -- print event

  -- Wait for a while before exiting
  threadDelay 10000000

  -- 4. Stop the client and disconnect
  stop handle
```

## Features
- **POSIX Unix Domain Socket Support**: Automatically finds Discord on Unix systems (`/tmp`, `$XDG_RUNTIME_DIR`).
- **Resilient Worker**: The IPC worker thread will safely retry connecting if Discord is closed, opened, or restarts.
- **Asynchronous Requests**: `Handle` interface routes replies to callers blocking on `call` using STMs without losing events.

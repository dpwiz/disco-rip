{-# LANGUAGE OverloadedStrings #-}
module DiscoRip.Client
  ( ClientConfig(..)
  , Handle(..)
  , startClient
  , waitReady
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, link, cancel, Async, race_)
import Control.Concurrent.STM
import Control.Exception (SomeException, try, bracket)
import Control.Monad (forever, when)
import Data.Aeson (encode, eitherDecode, Value)
import Data.ByteString.Lazy qualified as BL
import Debug.Trace (traceM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID.V4 (nextRandom)
import Network.Socket (close, Socket)

import DiscoRip.Frame qualified as Frame
import DiscoRip.Frame (readFrame, writeFrame, Opcode(..))
import DiscoRip.Message
import DiscoRip.Socket

data ClientConfig = ClientConfig
  { clientId :: Text
  , trace :: Bool
  } deriving (Show, Eq)

data Handle = Handle
  { clientAsync :: Async ()
  , cast :: Request Value -> IO ()
  , call :: Request Value -> IO Response
  , eventsQueue :: TBQueue Event
  , ready :: TVar Bool
  , stop :: IO ()
  }

startClient :: ClientConfig -> IO Handle
startClient config = do
  events <- newTBQueueIO 16
  -- We'll use Map Text Response to map from nonce to Response
  calls <- newTVarIO (mempty :: Map Text Response)
  writeQ <- newTBQueueIO 16
  readyVar <- newTVarIO False

  workerAsync <- async $ workerLoop config writeQ calls events readyVar
  link workerAsync

  let
    castImpl req = do
      uuid <- nextRandom
      let
        nonce = T.pack (show uuid)
        req' = req { reqNonce = nonce }
      traceLog config $ "Casting request: " <> show req'
      atomically $ writeTBQueue writeQ req'

    callImpl req = do
      uuid <- nextRandom
      let
        nonce = T.pack (show uuid)
        req' = req { reqNonce = nonce }

      traceLog config $ "Calling request: " <> show req'
      atomically $ writeTBQueue writeQ req'
      replyVar <- newTVarIO undefined

      let
        popOrRetry = Map.alterF \case
          Just r -> Nothing <$ writeTVar replyVar r
          Nothing -> retry

      traceLog config $ "Waiting for response with nonce: " <> show nonce
      atomically $ readTVar calls >>= popOrRetry nonce >>= writeTVar calls
      res <- readTVarIO replyVar
      traceLog config $ "Received response for nonce " <> show nonce <> ": " <> show res
      pure res

  pure Handle
    { clientAsync = workerAsync
    , cast = castImpl
    , call = callImpl
    , eventsQueue = events
    , ready = readyVar
    , stop = cancel workerAsync
    }

waitReady :: Handle -> IO ()
waitReady h = atomically $ do
  r <- readTVar (ready h)
  if r then pure () else retry

workerLoop :: ClientConfig -> TBQueue (Request Value) -> TVar (Map Text Response) -> TBQueue Event -> TVar Bool -> IO ()
workerLoop config writeQ calls events readyVar = forever $ do
  traceLog config "Searching for IPC socket..."
  mPath <- findIpcSocket
  case mPath of
    Nothing -> do
      traceLog config "IPC socket not found. Retrying in 5 seconds..."
      threadDelay 5000000 -- 5 seconds before retrying
    Just path -> do
      traceLog config $ "Found IPC socket at: " <> path
      res <- try @SomeException $ bracket
        (connectIpc path)
        (\sock -> do
          traceLog config "Closing IPC socket connection."
          atomically $ writeTVar readyVar False
          close sock)
        (\sock -> do
          traceLog config "Connected. Sending handshake."
          let
            hs = DiscoRip.Message.Handshake 1 (clientId config)
          writeFrame sock Frame.Handshake (encode hs)
          atomically $ writeTVar readyVar True

          -- After handshake, start reading and writing concurrently.
          -- If either throws an exception (e.g. pipe closed), they both die and we reconnect.
          race_ (readerLoop config sock calls events) (writerLoop config sock writeQ)
        )
      case res of
        Left err -> do
          traceLog config $ "Connection error or disconnected: " <> show err
          atomically $ writeTVar readyVar False
          threadDelay 5000000 -- wait before reconnect
        Right _ -> do
          traceLog config "Connection closed cleanly."
          pure ()

readerLoop :: ClientConfig -> Socket -> TVar (Map Text Response) -> TBQueue Event -> IO ()
readerLoop config sock calls events = forever $ do
  (op, payload) <- readFrame sock
  traceLog config $ "Received frame (Opcode: " <> show op <> ", size: " <> show (BL.length payload) <> "): " <> show payload
  case op of
    Frame -> do
      -- Try to parse as Response first
      case eitherDecode payload of
        Right (res :: Response) ->
          if T.null (resNonce res) then
            tryParseEvent payload
          else do
            traceLog config $ "Parsed Response: " <> show res
            atomically $ modifyTVar' calls $ Map.insert (resNonce res) res
        Left _ -> tryParseEvent payload
    Close -> do
      traceLog config "Discord requested close"
      error "Discord requested close"
    Ping -> do
      traceLog config "Received Ping, responding with Pong."
      -- respond with Pong
      writeFrame sock Pong payload
    _ -> pure ()
  where
    tryParseEvent payload =
      case eitherDecode payload of
        Right (evt :: Event) -> do
          traceLog config $ "Parsed Event: " <> show evt
          atomically $ writeTBQueue events evt
        Left err -> traceLog config $ "Failed to parse frame payload as Event or Response: " <> err

writerLoop :: ClientConfig -> Socket -> TBQueue (Request Value) -> IO ()
writerLoop config sock writeQ = forever $ do
  req <- atomically $ readTBQueue writeQ
  let encodedReq = encode req
  traceLog config $ "Writing frame (Opcode: Frame, size: " <> show (BL.length encodedReq) <> "): " <> show encodedReq
  writeFrame sock Frame encodedReq

traceLog :: ClientConfig -> String -> IO ()
traceLog config msg = when (trace config) (traceM $ "[DiscoRip] " <> msg)

{-# LANGUAGE OverloadedStrings #-}
module DiscoRip.Client
  ( ClientConfig(..)
  , Handle(..)
  , start
  , stop
  , waitReady
  ) where

import Control.Concurrent.STM

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, race_)
import Control.Exception (SomeException, try, bracket)
import Control.Monad (forever, when)
import Data.Aeson (encode, eitherDecode, Value)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID.V4 (nextRandom)
import Debug.Trace (traceM)
import Network.Socket (close, Socket)

import DiscoRip.Frame (readFrame, writeFrame, Opcode(..))
import DiscoRip.Frame qualified as Frame
import DiscoRip.Message qualified as Message
import DiscoRip.Socket qualified as Socket
import System.Posix (getProcessID)

data ClientConfig = ClientConfig
  { clientId :: Text
  , trace :: Bool
  } deriving (Show, Eq)

data Handle = Handle
  { pid :: Int
  , worker :: Async ()
  , cast :: Message.Request Value -> IO ()
  , call :: Message.Request Value -> IO Message.Response
  , ready :: TVar Bool
  }

start :: ClientConfig -> (Message.Event -> IO ())-> IO Handle
start config onEvent = do
  pid <- fromIntegral <$> getProcessID
  calls <- newTVarIO (mempty :: Map Text Message.Response)
  writeQ <- newTBQueueIO 16
  readyVar <- newTVarIO False

  worker <- async $ workerLoop config writeQ calls onEvent readyVar

  let
    castImpl req = do
      uuid <- nextRandom
      let req' = req {Message.reqNonce = T.pack (show uuid)}
      traceLog config $ "Casting request: " <> show req'
      atomically $ writeTBQueue writeQ req'

    callImpl req = do
      uuid <- nextRandom
      let
        nonce = T.pack (show uuid)
        req' = req { Message.reqNonce = nonce }
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
    { pid
    , worker = worker
    , cast = castImpl
    , call = callImpl
    , ready = readyVar
    }

stop :: Handle -> IO ()
stop Handle{worker} = cancel worker

waitReady :: Handle -> IO ()
waitReady Handle{ready} = atomically $ readTVar ready >>= check

workerLoop
  :: ClientConfig
  -> TBQueue (Message.Request Value)
  -> TVar (Map Text Message.Response)
  -> (Message.Event -> IO ())
  -> TVar Bool
  -> IO ()
workerLoop config@ClientConfig{..} writeQ calls onEvent readyVar = forever $ do
  traceLog config "Searching for IPC socket..."
  mPath <- Socket.findIpcSocket
  case mPath of
    Nothing -> do
      traceLog config "IPC socket not found. Retrying in 5 seconds..."
      threadDelay 5000000 -- 5 seconds before retrying
    Just path -> do
      traceLog config $ "Found IPC socket at: " <> path
      res <- try @SomeException $ bracket
        (Socket.connectIpc path)
        (\sock -> do
          traceLog config "Closing IPC socket connection."
          atomically $ writeTVar readyVar False
          close sock)
        (\sock -> do
          traceLog config "Connected. Sending handshake."
          let
            hs = Message.Handshake 1 clientId
          writeFrame sock Frame.Handshake (encode hs)
          atomically $ writeTVar readyVar True

          -- After handshake, start reading and writing concurrently.
          -- If either throws an exception (e.g. pipe closed), they both die and we reconnect.
          race_ (readerLoop config sock calls onEvent) (writerLoop config sock writeQ)
        )
      case res of
        Left err -> do
          traceLog config $ "Connection error or disconnected: " <> show err
          atomically $ writeTVar readyVar False
          threadDelay 5000000 -- wait before reconnect
        Right _ -> do
          traceLog config "Connection closed cleanly."
          pure ()

readerLoop :: ClientConfig -> Socket -> TVar (Map Text Message.Response) -> (Message.Event -> IO ()) -> IO ()
readerLoop config sock calls onEvent = forever $ do
  (op, payload) <- readFrame sock
  traceLog config $ "Received frame (Opcode: " <> show op <> ", size: " <> show (BSL.length payload) <> "): " <> show payload
  case op of
    Frame -> do
      -- Try to parse as Response first
      case eitherDecode payload of
        Right res@Message.Response{resNonce} ->
          if T.null resNonce then
            tryParseEvent payload
          else do
            traceLog config $ "Parsed Response: " <> show res
            atomically $ modifyTVar' calls $ Map.insert resNonce res
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
        Right evt -> do
          traceLog config $ "Parsed Event: " <> show evt
          onEvent evt
        Left err ->
          traceLog config $ "Failed to parse frame payload as Event or Response: " <> err

writerLoop :: ClientConfig -> Socket -> TBQueue (Message.Request Value) -> IO ()
writerLoop config sock writeQ = forever $ do
  req <- atomically $ readTBQueue writeQ
  let encodedReq = encode req
  traceLog config $ "Writing frame (Opcode: Frame, size: " <> show (BSL.length encodedReq) <> "): " <> show encodedReq
  writeFrame sock Frame encodedReq

traceLog :: ClientConfig -> String -> IO ()
traceLog ClientConfig{trace} msg = when trace (traceM $ "[DiscoRip] " <> msg)

{-# LANGUAGE CPP #-}
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
import System.IO (hClose)
import System.Random (randomRIO)
import qualified System.IO as SIO

import DiscoRip.Frame (readFrame, writeFrame, Opcode(..))
import DiscoRip.Frame qualified as Frame
import DiscoRip.Message qualified as Message
import DiscoRip.Socket qualified as Socket

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import System.Win32.Process (getCurrentProcessId)
#else
import System.Posix.Process (getProcessID)
#endif

myGetProcessID :: IO Int
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
myGetProcessID = fromIntegral <$> getCurrentProcessId
#else
myGetProcessID = fromIntegral <$> getProcessID
#endif

data ClientConfig = ClientConfig
  { clientId :: Text
  , trace :: Bool
  , reconnect :: Bool
  } deriving (Show, Eq)

data Handle = Handle
  { pid :: Int
  , worker :: Async ()
  , cast :: Message.Request Value -> IO ()
  , call :: Message.Request Value -> IO Message.Response
  , ready :: TVar Bool
  , reconnectVar :: TVar Bool
  }

start :: ClientConfig -> (Message.Event -> IO ())-> IO Handle
start config@ClientConfig{reconnect} onEvent = do
  pid <- myGetProcessID
  calls <- newTVarIO (mempty :: Map Text Message.Response)
  writeQ <- newTBQueueIO 16
  readyVar <- newTVarIO False
  reconnectVar <- newTVarIO reconnect

  worker <- async $ workerLoop config writeQ calls onEvent readyVar reconnectVar

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
    , reconnectVar = reconnectVar
    }

stop :: Handle -> IO ()
stop Handle{worker, reconnectVar} = do
  atomically $ writeTVar reconnectVar False
  cancel worker

waitReady :: Handle -> IO ()
waitReady Handle{ready} = atomically $ readTVar ready >>= check

workerLoop
  :: ClientConfig
  -> TBQueue (Message.Request Value)
  -> TVar (Map Text Message.Response)
  -> (Message.Event -> IO ())
  -> TVar Bool
  -> TVar Bool
  -> IO ()
workerLoop config@ClientConfig{..} writeQ calls onEvent readyVar reconnectVar = loop 1.0
  where
    loop :: Double -> IO ()
    loop delay = do
      shouldRun <- atomically $ readTVar reconnectVar
      when shouldRun $ do
        traceLog config "Searching for IPC socket..."
        mPath <- Socket.findIpcSocket
        case mPath of
          Nothing -> do
            traceLog config $ "IPC socket not found. Retrying in " <> show delay <> " seconds..."
            sleepWithJitter delay
            loop (min 60.0 (delay * 2))
          Just path -> do
            traceLog config $ "Found IPC socket at: " <> path
            res <- try @SomeException $ bracket
              (Socket.connectIpc path)
              (\h -> do
                traceLog config "Closing IPC socket connection."
                atomically $ writeTVar readyVar False
                hClose h)
              (\h -> do
                traceLog config "Connected. Sending handshake."
                let
                  hs = Message.Handshake 1 clientId
                writeFrame h Frame.Handshake (encode hs)
                waitReadyEvent config h onEvent
                atomically $ writeTVar readyVar True

                -- After handshake, start reading and writing concurrently.
                -- If either throws an exception (e.g. pipe closed), they both die and we reconnect.
                race_ (readerLoop config h calls onEvent) (writerLoop config h readyVar writeQ)
              )
            case res of
              Left err -> do
                traceLog config $ "Connection error or disconnected: " <> show err
                atomically $ writeTVar readyVar False
                shouldRun' <- atomically $ readTVar reconnectVar
                when shouldRun' $ do
                  sleepWithJitter delay
                  loop (min 60.0 (delay * 2))
              Right _ -> do
                traceLog config "Connection closed cleanly."
                shouldRun' <- atomically $ readTVar reconnectVar
                when shouldRun' $ do
                  -- reset backoff
                  sleepWithJitter 1.0
                  loop 1.0

    sleepWithJitter :: Double -> IO ()
    sleepWithJitter delaySec = do
      let microSec = delaySec * 1000000
      jitter <- randomRIO (-0.01 * microSec, 0.01 * microSec)
      threadDelay (round (microSec + jitter))

waitReadyEvent :: ClientConfig -> SIO.Handle -> (Message.Event -> IO ()) -> IO ()
waitReadyEvent config h onEvent = do
  traceLog config "Waiting for READY event"
  readFrame h >>= \case
    (Frame, payload) ->
      case eitherDecode payload of
        Right evt@Message.Event{evtEvt="READY"} -> do
          traceLog config "Got READY event"
          onEvent evt
        huh ->
          error $ show huh
    (op, payload) ->
      error $ show (op, payload)

readerLoop :: ClientConfig -> SIO.Handle -> TVar (Map Text Message.Response) -> (Message.Event -> IO ()) -> IO ()
readerLoop config h calls onEvent = forever $ do
  (op, payload) <- readFrame h
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
        Left _ ->
          tryParseEvent payload
    Close -> do
      traceLog config "Discord requested close"
      error "Discord requested close"
    Ping -> do
      traceLog config "Received Ping, responding with Pong."
      -- respond with Pong
      writeFrame h Pong payload
    _ -> pure ()
  where
    tryParseEvent payload =
      case eitherDecode payload of
        Right evt -> do
          traceLog config $ "Parsed Event: " <> show evt
          onEvent evt
        Left err ->
          traceLog config $ "Failed to parse frame payload as Event or Response: " <> err

writerLoop :: ClientConfig -> SIO.Handle -> TVar Bool -> TBQueue (Message.Request Value) -> IO ()
writerLoop config h ready writeQ = do
  -- wait for handshake first
  atomically $ readTVar ready >>= check
  forever do
    req <- atomically $ readTBQueue writeQ
    let encodedReq = encode req
    traceLog config $ "Writing frame (Opcode: Frame, size: " <> show (BSL.length encodedReq) <> "): " <> show encodedReq
    writeFrame h Frame encodedReq

traceLog :: ClientConfig -> String -> IO ()
traceLog ClientConfig{trace} msg = when trace (traceM $ "[DiscoRip] " <> msg)

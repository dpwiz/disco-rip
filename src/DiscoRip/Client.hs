{-# LANGUAGE OverloadedStrings #-}
module DiscoRip.Client
  ( Handle(..)
  , startClient
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, link, cancel, Async, race_)
import Control.Concurrent.STM
import Control.Exception (SomeException, try, bracket)
import Control.Monad (forever)
import Data.Aeson (encode, eitherDecode, Value)
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

data Handle = Handle
  { clientAsync :: Async ()
  , cast :: Request Value -> IO ()
  , call :: Request Value -> IO Response
  , eventsQueue :: TBQueue Event
  , stop :: IO ()
  }

startClient :: Text -> IO Handle
startClient clientId = do
  events <- newTBQueueIO 16
  -- We'll use Map Text Response to map from nonce to Response
  calls <- newTVarIO (mempty :: Map Text Response)
  writeQ <- newTBQueueIO 16

  workerAsync <- async $ workerLoop clientId writeQ calls events
  link workerAsync

  let
    castImpl req = do
      uuid <- nextRandom
      let nonce = T.pack (show uuid)
          req' = req { reqNonce = nonce }
      atomically $ writeTBQueue writeQ req'

    callImpl req = do
      uuid <- nextRandom
      let nonce = T.pack (show uuid)
          req' = req { reqNonce = nonce }

      atomically $ writeTBQueue writeQ req'
      replyVar <- newTVarIO undefined

      let
        popOrRetry = Map.alterF \case
          Just r -> Nothing <$ writeTVar replyVar r
          Nothing -> retry

      atomically $ readTVar calls >>= popOrRetry nonce >>= writeTVar calls
      readTVarIO replyVar

  pure $ Handle
    { clientAsync = workerAsync
    , cast = castImpl
    , call = callImpl
    , eventsQueue = events
    , stop = cancel workerAsync
    }

workerLoop :: Text -> TBQueue (Request Value) -> TVar (Map Text Response) -> TBQueue Event -> IO ()
workerLoop clientId writeQ calls events = forever $ do
  mPath <- findIpcSocket
  case mPath of
    Nothing -> do
      threadDelay 5000000 -- 5 seconds before retrying
    Just path -> do
      res <- try @SomeException $ bracket
        (connectIpc path)
        close
        (\sock -> do
          let hs = DiscoRip.Message.Handshake 1 clientId
          writeFrame sock Frame.Handshake (encode hs)

          -- After handshake, start reading and writing concurrently.
          -- If either throws an exception (e.g. pipe closed), they both die and we reconnect.
          race_ (readerLoop sock calls events) (writerLoop sock writeQ)
        )
      case res of
        Left _err -> threadDelay 5000000 -- wait before reconnect
        Right _ -> pure ()

readerLoop :: Socket -> TVar (Map Text Response) -> TBQueue Event -> IO ()
readerLoop sock calls events = forever $ do
  (op, payload) <- readFrame sock
  case op of
    Frame -> do
      -- Try to parse as Response first
      case eitherDecode payload of
        Right (res :: Response) ->
          if T.null (resNonce res)
          then tryParseEvent payload
          else atomically $ modifyTVar' calls $ Map.insert (resNonce res) res
        Left _ -> tryParseEvent payload
    Close -> error "Discord requested close"
    Ping -> do
      -- respond with Pong
      writeFrame sock Pong payload
    _ -> pure ()
  where
    tryParseEvent payload =
      case eitherDecode payload of
        Right (evt :: Event) -> atomically $ writeTBQueue events evt
        Left _ -> pure ()

writerLoop :: Socket -> TBQueue (Request Value) -> IO ()
writerLoop sock writeQ = forever $ do
  req <- atomically $ readTBQueue writeQ
  writeFrame sock Frame (encode req)

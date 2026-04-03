{-# LANGUAGE CPP #-}
module DiscoRip.Socket
  ( findIpcSocket
  , connectIpc
  ) where

import Control.Exception (try, IOException, bracket)
import Data.Maybe (catMaybes)
import System.IO (Handle, IOMode(..), openFile, hClose, hSetBuffering, BufferMode(..))
import System.Environment (lookupEnv)
import System.FilePath ((</>))

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

findIpcSocket :: IO (Maybe FilePath)
findIpcSocket = do
  paths <- getSocketPaths
  findFirstConnectable paths

connectIpc :: FilePath -> IO Handle
connectIpc path = do
  h <- openFile path ReadWriteMode
  hSetBuffering h NoBuffering
  pure h

getSocketPaths :: IO [FilePath]
getSocketPaths = pure [ "\\\\.\\pipe\\discord-ipc-" <> show (i :: Int) | i <- [0..9] ]

#else

import Network.Socket

findIpcSocket :: IO (Maybe FilePath)
findIpcSocket = do
  paths <- getSocketPaths
  -- Not using doesFileExist since it might be a socket file and doesFileExist can return False for sockets on some systems,
  -- but generally for Unix Domain sockets doesFileExist is False, however doesFileExist may work.
  -- Actually, let's just try to connect to each in order, as that's the most reliable way.
  -- To be safe, we will just return the first one we can successfully connect to.
  findFirstConnectable paths

connectIpc :: FilePath -> IO Handle
connectIpc path = do
  sock <- socket AF_UNIX Stream 0
  connect sock (SockAddrUnix path)
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering
  pure h

getSearchPaths :: IO [FilePath]
getSearchPaths = do
  xdgRuntime <- lookupEnv "XDG_RUNTIME_DIR"
  tmpDir <- lookupEnv "TMPDIR"
  tmp <- lookupEnv "TMP"
  pure (catMaybes [xdgRuntime, tmpDir, tmp] ++ fallbacks)
  where
    fallbacks = ["/tmp", "/run"]

getSocketPaths :: IO [FilePath]
getSocketPaths = do
  bases <- getSearchPaths
  pure [base </> sock | base <- bases, sock <- sockets]
  where
    sockets = ["discord-ipc-" <> show (i :: Int) | i <- [0..9]]

#endif

findFirstConnectable :: [FilePath] -> IO (Maybe FilePath)
findFirstConnectable [] = pure Nothing
findFirstConnectable (p:ps) = do
  res <- tryConnect p
  if res then
    pure (Just p)
  else
    findFirstConnectable ps

tryConnect :: FilePath -> IO Bool
tryConnect path = do
  res <- try @IOException do
    bracket (connectIpc path) hClose (\_ -> pure ())
  case res of
    Left _ -> pure False
    Right _ -> pure True

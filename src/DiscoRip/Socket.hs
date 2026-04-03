module DiscoRip.Socket
  ( findIpcSocket
  , connectIpc
  ) where

import Control.Exception (try, IOException)
import Data.Maybe (catMaybes)
import Network.Socket
import System.Environment (lookupEnv)
import System.FilePath ((</>))

findIpcSocket :: IO (Maybe FilePath)
findIpcSocket = do
  paths <- getSocketPaths
  -- Not using doesFileExist since it might be a socket file and doesFileExist can return False for sockets on some systems,
  -- but generally for Unix Domain sockets doesFileExist is False, however doesFileExist may work.
  -- Actually, let's just try to connect to each in order, as that's the most reliable way.
  -- To be safe, we will just return the first one we can successfully connect to.
  findFirstConnectable paths

connectIpc :: FilePath -> IO Socket
connectIpc path = do
  sock <- socket AF_UNIX Stream 0
  connect sock (SockAddrUnix path)
  pure sock

getSearchPaths :: IO [FilePath]
getSearchPaths = do
  xdgRuntime <- lookupEnv "XDG_RUNTIME_DIR"
  tmpDir <- lookupEnv "TMPDIR"
  tmp <- lookupEnv "TMP"
  pure (catMaybes [xdgRuntime, tmpDir, tmp] ++ fallbacks)
  where
    fallbacks = ["/tmp"]

getSocketPaths :: IO [FilePath]
getSocketPaths = do
  bases <- getSearchPaths
  pure [base </> sock | base <- bases, sock <- sockets]
  where
    sockets = ["discord-ipc-" <> show (i :: Int) | i <- [0..9]]

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
    sock <- socket AF_UNIX Stream 0
    connect sock (SockAddrUnix path)
    close sock
  case res of
    Left _ -> pure False
    Right _ -> pure True

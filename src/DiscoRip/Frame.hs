module DiscoRip.Frame
  ( Opcode(..)
  , FrameHeader(..)
  , encodeFrame
  , decodeFrameHeader
  , readFrame
  , writeFrame
  ) where

import Data.Binary (Binary(..), encode, decodeOrFail)
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Internal qualified as BLI
import Data.ByteString qualified as BS
import Data.Word (Word32)
import System.IO (Handle)

data Opcode
  = Handshake
  | Frame
  | Close
  | Ping
  | Pong
  deriving (Show, Eq, Enum)

instance Binary Opcode where
  put op = putWord32le $ case op of
    Handshake -> 0
    Frame     -> 1
    Close     -> 2
    Ping      -> 3
    Pong      -> 4
  get = do
    op <- getWord32le
    case op of
      0 -> pure Handshake
      1 -> pure Frame
      2 -> pure Close
      3 -> pure Ping
      4 -> pure Pong
      _ -> fail $ "Unknown Opcode: " <> show op

data FrameHeader = FrameHeader
  { fhOpcode :: Opcode
  , fhLength :: Word32
  } deriving (Show, Eq)

instance Binary FrameHeader where
  put (FrameHeader op len) = do
    put op
    putWord32le len
  get = FrameHeader <$> get <*> getWord32le

encodeFrame :: Opcode -> BL.ByteString -> BL.ByteString
encodeFrame op payload = header <> payload
  where
    len = fromIntegral (BL.length payload)
    header = encode (FrameHeader op len)

decodeFrameHeader :: BL.ByteString -> Either String (FrameHeader, BL.ByteString)
decodeFrameHeader bs =
  case decodeOrFail bs of
    Left (_, _, err) -> Left err
    Right (rest, _, header) -> Right (header, rest)

readFrame :: Handle -> IO (Opcode, BL.ByteString)
readFrame h = do
  headerBytes <- recvExactly h 8
  case decodeFrameHeader headerBytes of
    Left err -> error $ "Failed to decode frame header: " <> err
    Right (FrameHeader op len, _) -> do
      payload <- recvExactly h (fromIntegral len)
      pure (op, payload)

writeFrame :: Handle -> Opcode -> BL.ByteString -> IO ()
writeFrame h op = BL.hPut h . encodeFrame op

recvExactly :: Handle -> Int -> IO BL.ByteString
recvExactly h n = go n
  where
    go 0 = pure BLI.Empty
    go k = do
      chunk <- BS.hGetSome h k
      if BS.null chunk then
        error "Handle closed unexpectedly while receiving"
      else do
        rest <- go (k - BS.length chunk)
        pure (BLI.Chunk chunk rest)

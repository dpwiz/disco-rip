module DiscoRip.Message
  ( Handshake(..)
  , Request(..)
  , Response(..)
  , Event(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data Handshake = Handshake
  { v :: Int
  , client_id :: Text
  } deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Request a = Request
  { reqCmd :: Text
  , reqArgs :: a
  , reqNonce :: Text
  } deriving stock (Show, Eq, Generic)

instance ToJSON a => ToJSON (Request a) where
  toJSON Request{..} = object
    [ "cmd" .= reqCmd
    , "args" .= reqArgs
    , "nonce" .= reqNonce
    ]

data Response = Response
  { resCmd :: Text
  , resData :: Maybe Value
  , resEvt :: Maybe Text
  , resNonce :: Text
  } deriving stock (Show, Eq, Generic)

instance FromJSON Response where
  parseJSON = withObject "Response" $ \v -> Response
    <$> v .: "cmd"
    <*> v .:? "data"
    <*> v .:? "evt"
    <*> v .: "nonce"

data Event = Event
  { evtCmd :: Text
  , evtEvt :: Text
  , evtData :: Maybe Value
  } deriving stock (Show, Eq, Generic)

instance FromJSON Event where
  parseJSON = withObject "Event" $ \v -> Event
    <$> v .: "cmd"
    <*> v .: "evt"
    <*> v .:? "data"

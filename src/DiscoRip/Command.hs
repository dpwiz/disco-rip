module DiscoRip.Command
  ( mkActivity
  , Activity(..)
  , ActivityTimestamps(..)
  , ActivityAssets(..)
  , ActivityParty(..)
  , ActivitySecrets(..)
  , ActivityButton(..)
  , setActivity
  , SetActivityArgs(..)
  ) where

import Data.Aeson

import Data.Aeson.Types (Pair)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

import DiscoRip.Message qualified as Message
import DiscoRip.Client qualified as Client

filterEmpty :: [Pair] -> Value
filterEmpty = object . filter (\(_, v) -> v /= Null && v /= String "")

data ActivityTimestamps = ActivityTimestamps
  { start :: Maybe Int
  , end :: Maybe Int
  } deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ActivityAssets = ActivityAssets
  { large_image :: Text
  , large_text :: Text
  , small_image :: Text
  , small_text :: Text
  } deriving stock (Show, Eq, Generic)

instance ToJSON ActivityAssets where
  toJSON ActivityAssets{..} = filterEmpty
    [ "large_image" .= large_image
    , "large_text" .= large_text
    , "small_image" .= small_image
    , "small_text" .= small_text
    ]

instance FromJSON ActivityAssets where
  parseJSON = withObject "ActivityAssets" $ \v -> ActivityAssets
    <$> v .:? "large_image" .!= ""
    <*> v .:? "large_text" .!= ""
    <*> v .:? "small_image" .!= ""
    <*> v .:? "small_text" .!= ""

data ActivityParty = ActivityParty
  { partyId :: Text
  , size :: Maybe [Int]
  } deriving stock (Show, Eq, Generic)

instance ToJSON ActivityParty where
  toJSON ActivityParty{..} = filterEmpty
    [ "id" .= partyId
    , "size" .= size
    ]

instance FromJSON ActivityParty where
  parseJSON = withObject "ActivityParty" $ \v -> ActivityParty
    <$> v .:? "id" .!= ""
    <*> v .:? "size"

data ActivitySecrets = ActivitySecrets
  { join :: Text
  , spectate :: Text
  , match :: Text
  } deriving stock (Show, Eq, Generic)

instance ToJSON ActivitySecrets where
  toJSON ActivitySecrets{..} = filterEmpty
    [ "join" .= join
    , "spectate" .= spectate
    , "match" .= match
    ]

instance FromJSON ActivitySecrets where
  parseJSON = withObject "ActivitySecrets" $ \v -> ActivitySecrets
    <$> v .:? "join" .!= ""
    <*> v .:? "spectate" .!= ""
    <*> v .:? "match" .!= ""

data ActivityButton = ActivityButton
  { label :: Text
  , url :: Text
  } deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Activity = Activity
  { state :: Text
  , details :: Text
  , timestamps :: Maybe ActivityTimestamps
  , assets :: Maybe ActivityAssets
  , party :: Maybe ActivityParty
  , secrets :: Maybe ActivitySecrets
  , instance_ :: Maybe Bool
  , buttons :: Maybe [ActivityButton]
  } deriving stock (Show, Eq, Generic)

mkActivity :: Maybe Text -> Maybe Text -> Activity
mkActivity st det = Activity
  { state = fromMaybe "  " st
  , details = fromMaybe "  " det
  , timestamps = Nothing
  , assets = Nothing
  , party = Nothing
  , secrets = Nothing
  , instance_ = Nothing
  , buttons = Nothing
  }

instance ToJSON Activity where
  toJSON Activity{..} = filterEmpty
    [ "state" .= state
    , "details" .= details
    , "timestamps" .= timestamps
    , "assets" .= assets
    , "party" .= party
    , "secrets" .= secrets
    , "instance" .= instance_
    , "buttons" .= buttons
    ]

instance FromJSON Activity where
  parseJSON = withObject "Activity" $ \v -> Activity
    <$> v .:? "state" .!= ""
    <*> v .:? "details" .!= ""
    <*> v .:? "timestamps"
    <*> v .:? "assets"
    <*> v .:? "party"
    <*> v .:? "secrets"
    <*> v .:? "instance"
    <*> v .:? "buttons"

data SetActivityArgs = SetActivityArgs
  { pid :: Int
  , activity :: Activity
  } deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

setActivity :: Client.Handle -> Activity -> IO Message.Response
setActivity Client.Handle{call, pid} act = do
  let args = SetActivityArgs pid act
  call $ Message.Request "SET_ACTIVITY" (toJSON args) ""

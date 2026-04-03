module DiscoRip.Command
  ( Activity(..)
  , ActivityTimestamps(..)
  , ActivityAssets(..)
  , ActivityParty(..)
  , ActivitySecrets(..)
  , ActivityButton(..)
  , SetActivityArgs(..)
  , setActivity
  , mkActivity
  ) where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Posix.Process (getProcessID)

import DiscoRip.Client
import DiscoRip.Message

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
  toJSON a = filterEmpty
    [ "large_image" .= large_image a
    , "large_text" .= large_text a
    , "small_image" .= small_image a
    , "small_text" .= small_text a
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
  toJSON a = filterEmpty
    [ "id" .= partyId a
    , "size" .= size a
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
  toJSON a = filterEmpty
    [ "join" .= join a
    , "spectate" .= spectate a
    , "match" .= match a
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

mkActivity :: Text -> Text -> Activity
mkActivity st det = Activity
  { state = st
  , details = det
  , timestamps = Nothing
  , assets = Nothing
  , party = Nothing
  , secrets = Nothing
  , instance_ = Nothing
  , buttons = Nothing
  }

instance ToJSON Activity where
  toJSON a = filterEmpty
    [ "state" .= state a
    , "details" .= details a
    , "timestamps" .= timestamps a
    , "assets" .= assets a
    , "party" .= party a
    , "secrets" .= secrets a
    , "instance" .= instance_ a
    , "buttons" .= buttons a
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

setActivity :: Handle -> Activity -> IO Response
setActivity h act = do
  pidNum <- getProcessID
  let
    args = SetActivityArgs (fromIntegral pidNum) act
    req = Request "SET_ACTIVITY" (toJSON args) ""
  call h req

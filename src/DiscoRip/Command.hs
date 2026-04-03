module DiscoRip.Command
  ( setActivity
  , Activity(..)
  , ActivityTimestamps(..)
  , ActivityAssets(..)
  , ActivityParty(..)
  , ActivitySecrets(..)
  , ActivityButton(..)
  , SetActivityArgs(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

import DiscoRip.Client
import DiscoRip.Message

data ActivityTimestamps = ActivityTimestamps
  { start :: Maybe Int
  , end :: Maybe Int
  } deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ActivityAssets = ActivityAssets
  { large_image :: Maybe Text
  , large_text :: Maybe Text
  , small_image :: Maybe Text
  , small_text :: Maybe Text
  } deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ActivityParty = ActivityParty
  { id :: Maybe Text
  , size :: Maybe [Int]
  } deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ActivitySecrets = ActivitySecrets
  { join :: Maybe Text
  , spectate :: Maybe Text
  , match :: Maybe Text
  } deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ActivityButton = ActivityButton
  { label :: Text
  , url :: Text
  } deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Activity = Activity
  { state :: Maybe Text
  , details :: Maybe Text
  , timestamps :: Maybe ActivityTimestamps
  , assets :: Maybe ActivityAssets
  , party :: Maybe ActivityParty
  , secrets :: Maybe ActivitySecrets
  , instance_ :: Maybe Bool
  , buttons :: Maybe [ActivityButton]
  } deriving stock (Show, Eq, Generic)

instance ToJSON Activity where
  toJSON a = object $ filter (\(_, v) -> v /= Null)
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
    <$> v .:? "state"
    <*> v .:? "details"
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

setActivity :: Handle -> SetActivityArgs -> IO Response
setActivity h args = do
  let req = Request "SET_ACTIVITY" (toJSON args) ""
  call h req

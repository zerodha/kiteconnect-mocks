{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module TriggerRange
    ( TriggerRange (..)
    , Datum (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data TriggerRange = TriggerRange
    { triggerRangeDataTriggerRange :: Maybe (HashMap Text Datum)
    , statusTriggerRange :: Maybe Text
    } deriving (Show)

data Datum = Datum
    { instrumentTokenDatum :: Maybe Int
    , lowerDatum :: Maybe Float
    , upperDatum :: Maybe Float
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe TriggerRange
decodeTopLevel = decode

instance ToJSON TriggerRange where
    toJSON (TriggerRange triggerRangeDataTriggerRange statusTriggerRange) =
        object
        [ "data" .= triggerRangeDataTriggerRange
        , "status" .= statusTriggerRange
        ]

instance FromJSON TriggerRange where
    parseJSON (Object v) = TriggerRange
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Datum where
    toJSON (Datum instrumentTokenDatum lowerDatum upperDatum) =
        object
        [ "instrument_token" .= instrumentTokenDatum
        , "lower" .= lowerDatum
        , "upper" .= upperDatum
        ]

instance FromJSON Datum where
    parseJSON (Object v) = Datum
        <$> v .:? "instrument_token"
        <*> v .:? "lower"
        <*> v .:? "upper"

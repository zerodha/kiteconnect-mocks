{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module TickerLtp
    ( TickerLtp (..)
    , TriggerRangeElement (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

type TickerLtp = Vector TriggerRangeElement

data TriggerRangeElement = TriggerRangeElement
    { instrumentTokenTriggerRangeElement :: Maybe Int
    , lastPriceTriggerRangeElement :: Maybe Int
    , modeTriggerRangeElement :: Maybe Text
    , tradableTriggerRangeElement :: Maybe Bool
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe TickerLtp
decodeTopLevel = decode

instance ToJSON TriggerRangeElement where
    toJSON (TriggerRangeElement instrumentTokenTriggerRangeElement lastPriceTriggerRangeElement modeTriggerRangeElement tradableTriggerRangeElement) =
        object
        [ "instrument_token" .= instrumentTokenTriggerRangeElement
        , "last_price" .= lastPriceTriggerRangeElement
        , "mode" .= modeTriggerRangeElement
        , "tradable" .= tradableTriggerRangeElement
        ]

instance FromJSON TriggerRangeElement where
    parseJSON (Object v) = TriggerRangeElement
        <$> v .:? "instrument_token"
        <*> v .:? "last_price"
        <*> v .:? "mode"
        <*> v .:? "tradable"

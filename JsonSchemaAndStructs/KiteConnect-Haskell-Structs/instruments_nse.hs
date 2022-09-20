{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module InstrumentsNse
    ( InstrumentsNse (..)
    , InstrumentsNseElement (..)
    , Exchange (..)
    , InstrumentType (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

type InstrumentsNse = Vector InstrumentsNseElement

data InstrumentsNseElement = InstrumentsNseElement
    { exchangeInstrumentsNseElement :: Maybe Exchange
    , exchangeTokenInstrumentsNseElement :: Maybe Int
    , expiryInstrumentsNseElement :: Maybe Text
    , instrumentTokenInstrumentsNseElement :: Maybe Int
    , instrumentTypeInstrumentsNseElement :: Maybe InstrumentType
    , lastPriceInstrumentsNseElement :: Maybe Int
    , lotSizeInstrumentsNseElement :: Maybe Int
    , nameInstrumentsNseElement :: Maybe Text
    , segmentInstrumentsNseElement :: Maybe Exchange
    , strikeInstrumentsNseElement :: Maybe Int
    , tickSizeInstrumentsNseElement :: Maybe Float
    , tradingsymbolInstrumentsNseElement :: Maybe Text
    } deriving (Show)

data Exchange
    = NseExchange
    deriving (Show)

data InstrumentType
    = EqInstrumentType
    deriving (Show)

decodeTopLevel :: ByteString -> Maybe InstrumentsNse
decodeTopLevel = decode

instance ToJSON InstrumentsNseElement where
    toJSON (InstrumentsNseElement exchangeInstrumentsNseElement exchangeTokenInstrumentsNseElement expiryInstrumentsNseElement instrumentTokenInstrumentsNseElement instrumentTypeInstrumentsNseElement lastPriceInstrumentsNseElement lotSizeInstrumentsNseElement nameInstrumentsNseElement segmentInstrumentsNseElement strikeInstrumentsNseElement tickSizeInstrumentsNseElement tradingsymbolInstrumentsNseElement) =
        object
        [ "exchange" .= exchangeInstrumentsNseElement
        , "exchange_token" .= exchangeTokenInstrumentsNseElement
        , "expiry" .= expiryInstrumentsNseElement
        , "instrument_token" .= instrumentTokenInstrumentsNseElement
        , "instrument_type" .= instrumentTypeInstrumentsNseElement
        , "last_price" .= lastPriceInstrumentsNseElement
        , "lot_size" .= lotSizeInstrumentsNseElement
        , "name" .= nameInstrumentsNseElement
        , "segment" .= segmentInstrumentsNseElement
        , "strike" .= strikeInstrumentsNseElement
        , "tick_size" .= tickSizeInstrumentsNseElement
        , "tradingsymbol" .= tradingsymbolInstrumentsNseElement
        ]

instance FromJSON InstrumentsNseElement where
    parseJSON (Object v) = InstrumentsNseElement
        <$> v .:? "exchange"
        <*> v .:? "exchange_token"
        <*> v .:? "expiry"
        <*> v .:? "instrument_token"
        <*> v .:? "instrument_type"
        <*> v .:? "last_price"
        <*> v .:? "lot_size"
        <*> v .:? "name"
        <*> v .:? "segment"
        <*> v .:? "strike"
        <*> v .:? "tick_size"
        <*> v .:? "tradingsymbol"

instance ToJSON Exchange where
    toJSON NseExchange = "NSE"

instance FromJSON Exchange where
    parseJSON = withText "Exchange" parseText
        where
            parseText "NSE" = return NseExchange

instance ToJSON InstrumentType where
    toJSON EqInstrumentType = "EQ"

instance FromJSON InstrumentType where
    parseJSON = withText "InstrumentType" parseText
        where
            parseText "EQ" = return EqInstrumentType

{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module InstrumentsAll
    ( InstrumentsAll (..)
    , InstrumentsAllElement (..)
    , Exchange (..)
    , InstrumentType (..)
    , Segment (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

type InstrumentsAll = Vector InstrumentsAllElement

data InstrumentsAllElement = InstrumentsAllElement
    { exchangeInstrumentsAllElement :: Maybe Exchange
    , exchangeTokenInstrumentsAllElement :: Maybe Int
    , expiryInstrumentsAllElement :: Maybe Text
    , instrumentTokenInstrumentsAllElement :: Maybe Int
    , instrumentTypeInstrumentsAllElement :: Maybe InstrumentType
    , lastPriceInstrumentsAllElement :: Maybe Float
    , lotSizeInstrumentsAllElement :: Maybe Int
    , nameInstrumentsAllElement :: Maybe Text
    , segmentInstrumentsAllElement :: Maybe Segment
    , strikeInstrumentsAllElement :: Maybe Int
    , tickSizeInstrumentsAllElement :: Maybe Float
    , tradingsymbolInstrumentsAllElement :: Maybe Text
    } deriving (Show)

data Exchange
    = ExchangeBSEExchange
    | ExchangeNSEExchange
    | NfoExchange
    deriving (Show)

data InstrumentType
    = CeInstrumentType
    | EqInstrumentType
    | PEInstrumentType
    deriving (Show)

data Segment
    = NfoOptSegment
    | SegmentBSESegment
    | SegmentNSESegment
    deriving (Show)

decodeTopLevel :: ByteString -> Maybe InstrumentsAll
decodeTopLevel = decode

instance ToJSON InstrumentsAllElement where
    toJSON (InstrumentsAllElement exchangeInstrumentsAllElement exchangeTokenInstrumentsAllElement expiryInstrumentsAllElement instrumentTokenInstrumentsAllElement instrumentTypeInstrumentsAllElement lastPriceInstrumentsAllElement lotSizeInstrumentsAllElement nameInstrumentsAllElement segmentInstrumentsAllElement strikeInstrumentsAllElement tickSizeInstrumentsAllElement tradingsymbolInstrumentsAllElement) =
        object
        [ "exchange" .= exchangeInstrumentsAllElement
        , "exchange_token" .= exchangeTokenInstrumentsAllElement
        , "expiry" .= expiryInstrumentsAllElement
        , "instrument_token" .= instrumentTokenInstrumentsAllElement
        , "instrument_type" .= instrumentTypeInstrumentsAllElement
        , "last_price" .= lastPriceInstrumentsAllElement
        , "lot_size" .= lotSizeInstrumentsAllElement
        , "name" .= nameInstrumentsAllElement
        , "segment" .= segmentInstrumentsAllElement
        , "strike" .= strikeInstrumentsAllElement
        , "tick_size" .= tickSizeInstrumentsAllElement
        , "tradingsymbol" .= tradingsymbolInstrumentsAllElement
        ]

instance FromJSON InstrumentsAllElement where
    parseJSON (Object v) = InstrumentsAllElement
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
    toJSON ExchangeBSEExchange = "BSE"
    toJSON ExchangeNSEExchange = "NSE"
    toJSON NfoExchange = "NFO"

instance FromJSON Exchange where
    parseJSON = withText "Exchange" parseText
        where
            parseText "BSE" = return ExchangeBSEExchange
            parseText "NSE" = return ExchangeNSEExchange
            parseText "NFO" = return NfoExchange

instance ToJSON InstrumentType where
    toJSON CeInstrumentType = "CE"
    toJSON EqInstrumentType = "EQ"
    toJSON PEInstrumentType = "PE"

instance FromJSON InstrumentType where
    parseJSON = withText "InstrumentType" parseText
        where
            parseText "CE" = return CeInstrumentType
            parseText "EQ" = return EqInstrumentType
            parseText "PE" = return PEInstrumentType

instance ToJSON Segment where
    toJSON NfoOptSegment = "NFO-OPT"
    toJSON SegmentBSESegment = "BSE"
    toJSON SegmentNSESegment = "NSE"

instance FromJSON Segment where
    parseJSON = withText "Segment" parseText
        where
            parseText "NFO-OPT" = return NfoOptSegment
            parseText "BSE" = return SegmentBSESegment
            parseText "NSE" = return SegmentNSESegment

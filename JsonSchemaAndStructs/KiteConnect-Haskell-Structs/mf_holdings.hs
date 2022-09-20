{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module MfHoldings
    ( MFHoldings (..)
    , Datum (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data MFHoldings = MFHoldings
    { mfHoldingsDataMFHoldings :: Maybe (Vector Datum)
    , statusMFHoldings :: Maybe Text
    } deriving (Show)

data Datum = Datum
    { averagePriceDatum :: Maybe Float
    , folioDatum :: Maybe Text
    , fundDatum :: Maybe Text
    , lastPriceDatum :: Maybe Float
    , lastPriceDateDatum :: Maybe Text
    , pledgedQuantityDatum :: Maybe Int
    , pnlDatum :: Maybe Int
    , quantityDatum :: Maybe Float
    , tradingsymbolDatum :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFHoldings
decodeTopLevel = decode

instance ToJSON MFHoldings where
    toJSON (MFHoldings mfHoldingsDataMFHoldings statusMFHoldings) =
        object
        [ "data" .= mfHoldingsDataMFHoldings
        , "status" .= statusMFHoldings
        ]

instance FromJSON MFHoldings where
    parseJSON (Object v) = MFHoldings
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Datum where
    toJSON (Datum averagePriceDatum folioDatum fundDatum lastPriceDatum lastPriceDateDatum pledgedQuantityDatum pnlDatum quantityDatum tradingsymbolDatum) =
        object
        [ "average_price" .= averagePriceDatum
        , "folio" .= folioDatum
        , "fund" .= fundDatum
        , "last_price" .= lastPriceDatum
        , "last_price_date" .= lastPriceDateDatum
        , "pledged_quantity" .= pledgedQuantityDatum
        , "pnl" .= pnlDatum
        , "quantity" .= quantityDatum
        , "tradingsymbol" .= tradingsymbolDatum
        ]

instance FromJSON Datum where
    parseJSON (Object v) = Datum
        <$> v .:? "average_price"
        <*> v .:? "folio"
        <*> v .:? "fund"
        <*> v .:? "last_price"
        <*> v .:? "last_price_date"
        <*> v .:? "pledged_quantity"
        <*> v .:? "pnl"
        <*> v .:? "quantity"
        <*> v .:? "tradingsymbol"

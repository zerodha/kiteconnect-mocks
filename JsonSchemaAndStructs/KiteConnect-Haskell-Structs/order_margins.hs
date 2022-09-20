{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderMargins
    ( OrderMargins (..)
    , Datum (..)
    , Pnl (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data OrderMargins = OrderMargins
    { orderMarginsDataOrderMargins :: Maybe (Vector Datum)
    , statusOrderMargins :: Maybe Text
    } deriving (Show)

data Datum = Datum
    { additionalDatum :: Maybe Int
    , boDatum :: Maybe Int
    , cashDatum :: Maybe Int
    , exchangeDatum :: Maybe Text
    , exposureDatum :: Maybe Int
    , optionPremiumDatum :: Maybe Int
    , pnlDatum :: Maybe Pnl
    , spanDatum :: Maybe Int
    , totalDatum :: Maybe Float
    , tradingsymbolDatum :: Maybe Text
    , datumTypeDatum :: Maybe Text
    , varDatum :: Maybe Float
    } deriving (Show)

data Pnl = Pnl
    { realisedPnl :: Maybe Int
    , unrealisedPnl :: Maybe Int
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe OrderMargins
decodeTopLevel = decode

instance ToJSON OrderMargins where
    toJSON (OrderMargins orderMarginsDataOrderMargins statusOrderMargins) =
        object
        [ "data" .= orderMarginsDataOrderMargins
        , "status" .= statusOrderMargins
        ]

instance FromJSON OrderMargins where
    parseJSON (Object v) = OrderMargins
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Datum where
    toJSON (Datum additionalDatum boDatum cashDatum exchangeDatum exposureDatum optionPremiumDatum pnlDatum spanDatum totalDatum tradingsymbolDatum datumTypeDatum varDatum) =
        object
        [ "additional" .= additionalDatum
        , "bo" .= boDatum
        , "cash" .= cashDatum
        , "exchange" .= exchangeDatum
        , "exposure" .= exposureDatum
        , "option_premium" .= optionPremiumDatum
        , "pnl" .= pnlDatum
        , "span" .= spanDatum
        , "total" .= totalDatum
        , "tradingsymbol" .= tradingsymbolDatum
        , "type" .= datumTypeDatum
        , "var" .= varDatum
        ]

instance FromJSON Datum where
    parseJSON (Object v) = Datum
        <$> v .:? "additional"
        <*> v .:? "bo"
        <*> v .:? "cash"
        <*> v .:? "exchange"
        <*> v .:? "exposure"
        <*> v .:? "option_premium"
        <*> v .:? "pnl"
        <*> v .:? "span"
        <*> v .:? "total"
        <*> v .:? "tradingsymbol"
        <*> v .:? "type"
        <*> v .:? "var"

instance ToJSON Pnl where
    toJSON (Pnl realisedPnl unrealisedPnl) =
        object
        [ "realised" .= realisedPnl
        , "unrealised" .= unrealisedPnl
        ]

instance FromJSON Pnl where
    parseJSON (Object v) = Pnl
        <$> v .:? "realised"
        <*> v .:? "unrealised"

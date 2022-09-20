{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module BasketMargins
    ( BasketMargins (..)
    , Data (..)
    , Final (..)
    , Pnl (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data BasketMargins = BasketMargins
    { basketMarginsDataBasketMargins :: Maybe Data
    , statusBasketMargins :: Maybe Text
    } deriving (Show)

data Data = Data
    { finalData :: Maybe Final
    , initialData :: Maybe Final
    , ordersData :: Maybe (Vector Final)
    } deriving (Show)

data Final = Final
    { additionalFinal :: Maybe Int
    , boFinal :: Maybe Int
    , cashFinal :: Maybe Int
    , exchangeFinal :: Maybe Text
    , exposureFinal :: Maybe Float
    , optionPremiumFinal :: Maybe Float
    , pnlFinal :: Maybe Pnl
    , spanFinal :: Maybe Float
    , totalFinal :: Maybe Float
    , tradingsymbolFinal :: Maybe Text
    , finalTypeFinal :: Maybe Text
    , varFinal :: Maybe Int
    } deriving (Show)

data Pnl = Pnl
    { realisedPnl :: Maybe Int
    , unrealisedPnl :: Maybe Int
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe BasketMargins
decodeTopLevel = decode

instance ToJSON BasketMargins where
    toJSON (BasketMargins basketMarginsDataBasketMargins statusBasketMargins) =
        object
        [ "data" .= basketMarginsDataBasketMargins
        , "status" .= statusBasketMargins
        ]

instance FromJSON BasketMargins where
    parseJSON (Object v) = BasketMargins
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Data where
    toJSON (Data finalData initialData ordersData) =
        object
        [ "final" .= finalData
        , "initial" .= initialData
        , "orders" .= ordersData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .:? "final"
        <*> v .:? "initial"
        <*> v .:? "orders"

instance ToJSON Final where
    toJSON (Final additionalFinal boFinal cashFinal exchangeFinal exposureFinal optionPremiumFinal pnlFinal spanFinal totalFinal tradingsymbolFinal finalTypeFinal varFinal) =
        object
        [ "additional" .= additionalFinal
        , "bo" .= boFinal
        , "cash" .= cashFinal
        , "exchange" .= exchangeFinal
        , "exposure" .= exposureFinal
        , "option_premium" .= optionPremiumFinal
        , "pnl" .= pnlFinal
        , "span" .= spanFinal
        , "total" .= totalFinal
        , "tradingsymbol" .= tradingsymbolFinal
        , "type" .= finalTypeFinal
        , "var" .= varFinal
        ]

instance FromJSON Final where
    parseJSON (Object v) = Final
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

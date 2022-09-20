{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module MarginCommodity
    ( MarginCommodity (..)
    , Data (..)
    , Available (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data MarginCommodity = MarginCommodity
    { marginCommodityDataMarginCommodity :: Maybe Data
    , statusMarginCommodity :: Maybe Text
    } deriving (Show)

data Data = Data
    { availableData :: Maybe Available
    , enabledData :: Maybe Bool
    , netData :: Maybe Float
    , utilisedData :: Maybe (HashMap Text Float)
    } deriving (Show)

data Available = Available
    { adhocMarginAvailable :: Maybe Int
    , cashAvailable :: Maybe Float
    , collateralAvailable :: Maybe Int
    , intradayPayinAvailable :: Maybe Int
    , liveBalanceAvailable :: Maybe Float
    , openingBalanceAvailable :: Maybe Float
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MarginCommodity
decodeTopLevel = decode

instance ToJSON MarginCommodity where
    toJSON (MarginCommodity marginCommodityDataMarginCommodity statusMarginCommodity) =
        object
        [ "data" .= marginCommodityDataMarginCommodity
        , "status" .= statusMarginCommodity
        ]

instance FromJSON MarginCommodity where
    parseJSON (Object v) = MarginCommodity
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Data where
    toJSON (Data availableData enabledData netData utilisedData) =
        object
        [ "available" .= availableData
        , "enabled" .= enabledData
        , "net" .= netData
        , "utilised" .= utilisedData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .:? "available"
        <*> v .:? "enabled"
        <*> v .:? "net"
        <*> v .:? "utilised"

instance ToJSON Available where
    toJSON (Available adhocMarginAvailable cashAvailable collateralAvailable intradayPayinAvailable liveBalanceAvailable openingBalanceAvailable) =
        object
        [ "adhoc_margin" .= adhocMarginAvailable
        , "cash" .= cashAvailable
        , "collateral" .= collateralAvailable
        , "intraday_payin" .= intradayPayinAvailable
        , "live_balance" .= liveBalanceAvailable
        , "opening_balance" .= openingBalanceAvailable
        ]

instance FromJSON Available where
    parseJSON (Object v) = Available
        <$> v .:? "adhoc_margin"
        <*> v .:? "cash"
        <*> v .:? "collateral"
        <*> v .:? "intraday_payin"
        <*> v .:? "live_balance"
        <*> v .:? "opening_balance"

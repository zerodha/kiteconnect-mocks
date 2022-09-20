{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module MarginsEquity
    ( MarginsEquity (..)
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

data MarginsEquity = MarginsEquity
    { marginsEquityDataMarginsEquity :: Maybe Data
    , statusMarginsEquity :: Maybe Text
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

decodeTopLevel :: ByteString -> Maybe MarginsEquity
decodeTopLevel = decode

instance ToJSON MarginsEquity where
    toJSON (MarginsEquity marginsEquityDataMarginsEquity statusMarginsEquity) =
        object
        [ "data" .= marginsEquityDataMarginsEquity
        , "status" .= statusMarginsEquity
        ]

instance FromJSON MarginsEquity where
    parseJSON (Object v) = MarginsEquity
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

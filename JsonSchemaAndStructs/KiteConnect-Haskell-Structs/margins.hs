{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Margins
    ( Margins (..)
    , Data (..)
    , Ity (..)
    , Available (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data Margins = Margins
    { marginsDataMargins :: Maybe Data
    , statusMargins :: Maybe Text
    } deriving (Show)

data Data = Data
    { commodityData :: Maybe Ity
    , equityData :: Maybe Ity
    } deriving (Show)

data Ity = Ity
    { availableIty :: Maybe Available
    , enabledIty :: Maybe Bool
    , netIty :: Maybe Float
    , utilisedIty :: Maybe (HashMap Text Float)
    } deriving (Show)

data Available = Available
    { adhocMarginAvailable :: Maybe Int
    , cashAvailable :: Maybe Float
    , collateralAvailable :: Maybe Int
    , intradayPayinAvailable :: Maybe Int
    , liveBalanceAvailable :: Maybe Float
    , openingBalanceAvailable :: Maybe Float
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Margins
decodeTopLevel = decode

instance ToJSON Margins where
    toJSON (Margins marginsDataMargins statusMargins) =
        object
        [ "data" .= marginsDataMargins
        , "status" .= statusMargins
        ]

instance FromJSON Margins where
    parseJSON (Object v) = Margins
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Data where
    toJSON (Data commodityData equityData) =
        object
        [ "commodity" .= commodityData
        , "equity" .= equityData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .:? "commodity"
        <*> v .:? "equity"

instance ToJSON Ity where
    toJSON (Ity availableIty enabledIty netIty utilisedIty) =
        object
        [ "available" .= availableIty
        , "enabled" .= enabledIty
        , "net" .= netIty
        , "utilised" .= utilisedIty
        ]

instance FromJSON Ity where
    parseJSON (Object v) = Ity
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

{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Holdings
    ( Holdings (..)
    , Datum (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data Holdings = Holdings
    { holdingsDataHoldings :: Maybe (Vector Datum)
    , statusHoldings :: Maybe Text
    } deriving (Show)

data Datum = Datum
    { authorisedDateDatum :: Maybe Text
    , authorisedQuantityDatum :: Maybe Int
    , averagePriceDatum :: Maybe Float
    , closePriceDatum :: Maybe Float
    , collateralQuantityDatum :: Maybe Int
    , collateralTypeDatum :: Maybe Text
    , dayChangeDatum :: Maybe Float
    , dayChangePercentageDatum :: Maybe Float
    , discrepancyDatum :: Maybe Bool
    , exchangeDatum :: Maybe Text
    , instrumentTokenDatum :: Maybe Int
    , isinDatum :: Maybe Text
    , lastPriceDatum :: Maybe Float
    , openingQuantityDatum :: Maybe Int
    , pnlDatum :: Maybe Float
    , priceDatum :: Maybe Int
    , productDatum :: Maybe Text
    , quantityDatum :: Maybe Int
    , realisedQuantityDatum :: Maybe Int
    , t1QuantityDatum :: Maybe Int
    , tradingsymbolDatum :: Maybe Text
    , usedQuantityDatum :: Maybe Int
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Holdings
decodeTopLevel = decode

instance ToJSON Holdings where
    toJSON (Holdings holdingsDataHoldings statusHoldings) =
        object
        [ "data" .= holdingsDataHoldings
        , "status" .= statusHoldings
        ]

instance FromJSON Holdings where
    parseJSON (Object v) = Holdings
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Datum where
    toJSON (Datum authorisedDateDatum authorisedQuantityDatum averagePriceDatum closePriceDatum collateralQuantityDatum collateralTypeDatum dayChangeDatum dayChangePercentageDatum discrepancyDatum exchangeDatum instrumentTokenDatum isinDatum lastPriceDatum openingQuantityDatum pnlDatum priceDatum productDatum quantityDatum realisedQuantityDatum t1QuantityDatum tradingsymbolDatum usedQuantityDatum) =
        object
        [ "authorised_date" .= authorisedDateDatum
        , "authorised_quantity" .= authorisedQuantityDatum
        , "average_price" .= averagePriceDatum
        , "close_price" .= closePriceDatum
        , "collateral_quantity" .= collateralQuantityDatum
        , "collateral_type" .= collateralTypeDatum
        , "day_change" .= dayChangeDatum
        , "day_change_percentage" .= dayChangePercentageDatum
        , "discrepancy" .= discrepancyDatum
        , "exchange" .= exchangeDatum
        , "instrument_token" .= instrumentTokenDatum
        , "isin" .= isinDatum
        , "last_price" .= lastPriceDatum
        , "opening_quantity" .= openingQuantityDatum
        , "pnl" .= pnlDatum
        , "price" .= priceDatum
        , "product" .= productDatum
        , "quantity" .= quantityDatum
        , "realised_quantity" .= realisedQuantityDatum
        , "t1_quantity" .= t1QuantityDatum
        , "tradingsymbol" .= tradingsymbolDatum
        , "used_quantity" .= usedQuantityDatum
        ]

instance FromJSON Datum where
    parseJSON (Object v) = Datum
        <$> v .:? "authorised_date"
        <*> v .:? "authorised_quantity"
        <*> v .:? "average_price"
        <*> v .:? "close_price"
        <*> v .:? "collateral_quantity"
        <*> v .:? "collateral_type"
        <*> v .:? "day_change"
        <*> v .:? "day_change_percentage"
        <*> v .:? "discrepancy"
        <*> v .:? "exchange"
        <*> v .:? "instrument_token"
        <*> v .:? "isin"
        <*> v .:? "last_price"
        <*> v .:? "opening_quantity"
        <*> v .:? "pnl"
        <*> v .:? "price"
        <*> v .:? "product"
        <*> v .:? "quantity"
        <*> v .:? "realised_quantity"
        <*> v .:? "t1_quantity"
        <*> v .:? "tradingsymbol"
        <*> v .:? "used_quantity"

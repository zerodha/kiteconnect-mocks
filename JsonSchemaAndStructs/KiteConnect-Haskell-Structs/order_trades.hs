{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( OrderTrades (..)
    , Definitions (..)
    , Datum (..)
    , DatumProperties (..)
    , AveragePrice (..)
    , ExchangeTimestamp (..)
    , OrderTradesClass (..)
    , OrderTradesProperties (..)
    , Data (..)
    , Items (..)
    , Type (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data OrderTrades = OrderTrades
    { refOrderTrades :: Text
    , schemaOrderTrades :: Text
    , definitionsOrderTrades :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { datumDefinitions :: Datum
    , orderTradesDefinitions :: OrderTradesClass
    } deriving (Show)

data Datum = Datum
    { additionalPropertiesDatum :: Bool
    , propertiesDatum :: DatumProperties
    , requiredDatum :: Vector Text
    , titleDatum :: Text
    , datumTypeDatum :: Text
    } deriving (Show)

data DatumProperties = DatumProperties
    { averagePriceDatumProperties :: AveragePrice
    , exchangeDatumProperties :: AveragePrice
    , exchangeOrderIDDatumProperties :: AveragePrice
    , exchangeTimestampDatumProperties :: ExchangeTimestamp
    , fillTimestampDatumProperties :: ExchangeTimestamp
    , instrumentTokenDatumProperties :: AveragePrice
    , orderIDDatumProperties :: AveragePrice
    , orderTimestampDatumProperties :: ExchangeTimestamp
    , productDatumProperties :: AveragePrice
    , quantityDatumProperties :: AveragePrice
    , tradeIDDatumProperties :: ExchangeTimestamp
    , tradingsymbolDatumProperties :: AveragePrice
    , transactionTypeDatumProperties :: AveragePrice
    } deriving (Show)

data AveragePrice = AveragePrice
    { averagePriceTypeAveragePrice :: Type
    } deriving (Show)

data Type
    = IntegerType
    | StringType
    deriving (Show)

data ExchangeTimestamp = ExchangeTimestamp
    { formatExchangeTimestamp :: Text
    , exchangeTimestampTypeExchangeTimestamp :: Type
    } deriving (Show)

data OrderTradesClass = OrderTradesClass
    { additionalPropertiesOrderTradesClass :: Bool
    , propertiesOrderTradesClass :: OrderTradesProperties
    , requiredOrderTradesClass :: Vector Text
    , titleOrderTradesClass :: Text
    , orderTradesClassTypeOrderTradesClass :: Text
    } deriving (Show)

data OrderTradesProperties = OrderTradesProperties
    { orderTradesPropertiesDataOrderTradesProperties :: Data
    , statusOrderTradesProperties :: AveragePrice
    } deriving (Show)

data Data = Data
    { itemsData :: Items
    , dataTypeData :: Text
    } deriving (Show)

data Items = Items
    { refItems :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe OrderTrades
decodeTopLevel = decode

instance ToJSON OrderTrades where
    toJSON (OrderTrades refOrderTrades schemaOrderTrades definitionsOrderTrades) =
        object
        [ "$ref" .= refOrderTrades
        , "$schema" .= schemaOrderTrades
        , "definitions" .= definitionsOrderTrades
        ]

instance FromJSON OrderTrades where
    parseJSON (Object v) = OrderTrades
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions datumDefinitions orderTradesDefinitions) =
        object
        [ "Datum" .= datumDefinitions
        , "OrderTrades" .= orderTradesDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Datum"
        <*> v .: "OrderTrades"

instance ToJSON Datum where
    toJSON (Datum additionalPropertiesDatum propertiesDatum requiredDatum titleDatum datumTypeDatum) =
        object
        [ "additionalProperties" .= additionalPropertiesDatum
        , "properties" .= propertiesDatum
        , "required" .= requiredDatum
        , "title" .= titleDatum
        , "type" .= datumTypeDatum
        ]

instance FromJSON Datum where
    parseJSON (Object v) = Datum
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON DatumProperties where
    toJSON (DatumProperties averagePriceDatumProperties exchangeDatumProperties exchangeOrderIDDatumProperties exchangeTimestampDatumProperties fillTimestampDatumProperties instrumentTokenDatumProperties orderIDDatumProperties orderTimestampDatumProperties productDatumProperties quantityDatumProperties tradeIDDatumProperties tradingsymbolDatumProperties transactionTypeDatumProperties) =
        object
        [ "average_price" .= averagePriceDatumProperties
        , "exchange" .= exchangeDatumProperties
        , "exchange_order_id" .= exchangeOrderIDDatumProperties
        , "exchange_timestamp" .= exchangeTimestampDatumProperties
        , "fill_timestamp" .= fillTimestampDatumProperties
        , "instrument_token" .= instrumentTokenDatumProperties
        , "order_id" .= orderIDDatumProperties
        , "order_timestamp" .= orderTimestampDatumProperties
        , "product" .= productDatumProperties
        , "quantity" .= quantityDatumProperties
        , "trade_id" .= tradeIDDatumProperties
        , "tradingsymbol" .= tradingsymbolDatumProperties
        , "transaction_type" .= transactionTypeDatumProperties
        ]

instance FromJSON DatumProperties where
    parseJSON (Object v) = DatumProperties
        <$> v .: "average_price"
        <*> v .: "exchange"
        <*> v .: "exchange_order_id"
        <*> v .: "exchange_timestamp"
        <*> v .: "fill_timestamp"
        <*> v .: "instrument_token"
        <*> v .: "order_id"
        <*> v .: "order_timestamp"
        <*> v .: "product"
        <*> v .: "quantity"
        <*> v .: "trade_id"
        <*> v .: "tradingsymbol"
        <*> v .: "transaction_type"

instance ToJSON AveragePrice where
    toJSON (AveragePrice averagePriceTypeAveragePrice) =
        object
        [ "type" .= averagePriceTypeAveragePrice
        ]

instance FromJSON AveragePrice where
    parseJSON (Object v) = AveragePrice
        <$> v .: "type"

instance ToJSON Type where
    toJSON IntegerType = "integer"
    toJSON StringType = "string"

instance FromJSON Type where
    parseJSON = withText "Type" parseText
        where
            parseText "integer" = return IntegerType
            parseText "string" = return StringType

instance ToJSON ExchangeTimestamp where
    toJSON (ExchangeTimestamp formatExchangeTimestamp exchangeTimestampTypeExchangeTimestamp) =
        object
        [ "format" .= formatExchangeTimestamp
        , "type" .= exchangeTimestampTypeExchangeTimestamp
        ]

instance FromJSON ExchangeTimestamp where
    parseJSON (Object v) = ExchangeTimestamp
        <$> v .: "format"
        <*> v .: "type"

instance ToJSON OrderTradesClass where
    toJSON (OrderTradesClass additionalPropertiesOrderTradesClass propertiesOrderTradesClass requiredOrderTradesClass titleOrderTradesClass orderTradesClassTypeOrderTradesClass) =
        object
        [ "additionalProperties" .= additionalPropertiesOrderTradesClass
        , "properties" .= propertiesOrderTradesClass
        , "required" .= requiredOrderTradesClass
        , "title" .= titleOrderTradesClass
        , "type" .= orderTradesClassTypeOrderTradesClass
        ]

instance FromJSON OrderTradesClass where
    parseJSON (Object v) = OrderTradesClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON OrderTradesProperties where
    toJSON (OrderTradesProperties orderTradesPropertiesDataOrderTradesProperties statusOrderTradesProperties) =
        object
        [ "data" .= orderTradesPropertiesDataOrderTradesProperties
        , "status" .= statusOrderTradesProperties
        ]

instance FromJSON OrderTradesProperties where
    parseJSON (Object v) = OrderTradesProperties
        <$> v .: "data"
        <*> v .: "status"

instance ToJSON Data where
    toJSON (Data itemsData dataTypeData) =
        object
        [ "items" .= itemsData
        , "type" .= dataTypeData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .: "items"
        <*> v .: "type"

instance ToJSON Items where
    toJSON (Items refItems) =
        object
        [ "$ref" .= refItems
        ]

instance FromJSON Items where
    parseJSON (Object v) = Items
        <$> v .: "$ref"

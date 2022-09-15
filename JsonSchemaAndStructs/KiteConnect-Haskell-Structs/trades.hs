{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( Trades (..)
    , Definitions (..)
    , Datum (..)
    , DatumProperties (..)
    , AveragePrice (..)
    , ExchangeTimestamp (..)
    , TradesClass (..)
    , TradesProperties (..)
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

data Trades = Trades
    { refTrades :: Text
    , schemaTrades :: Text
    , definitionsTrades :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { datumDefinitions :: Datum
    , tradesDefinitions :: TradesClass
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
    | NumberType
    | StringType
    deriving (Show)

data ExchangeTimestamp = ExchangeTimestamp
    { formatExchangeTimestamp :: Text
    , exchangeTimestampTypeExchangeTimestamp :: Type
    } deriving (Show)

data TradesClass = TradesClass
    { additionalPropertiesTradesClass :: Bool
    , propertiesTradesClass :: TradesProperties
    , requiredTradesClass :: Vector Text
    , titleTradesClass :: Text
    , tradesClassTypeTradesClass :: Text
    } deriving (Show)

data TradesProperties = TradesProperties
    { tradesPropertiesDataTradesProperties :: Data
    , statusTradesProperties :: AveragePrice
    } deriving (Show)

data Data = Data
    { itemsData :: Items
    , dataTypeData :: Text
    } deriving (Show)

data Items = Items
    { refItems :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Trades
decodeTopLevel = decode

instance ToJSON Trades where
    toJSON (Trades refTrades schemaTrades definitionsTrades) =
        object
        [ "$ref" .= refTrades
        , "$schema" .= schemaTrades
        , "definitions" .= definitionsTrades
        ]

instance FromJSON Trades where
    parseJSON (Object v) = Trades
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions datumDefinitions tradesDefinitions) =
        object
        [ "Datum" .= datumDefinitions
        , "Trades" .= tradesDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Datum"
        <*> v .: "Trades"

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
    toJSON NumberType = "number"
    toJSON StringType = "string"

instance FromJSON Type where
    parseJSON = withText "Type" parseText
        where
            parseText "integer" = return IntegerType
            parseText "number" = return NumberType
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

instance ToJSON TradesClass where
    toJSON (TradesClass additionalPropertiesTradesClass propertiesTradesClass requiredTradesClass titleTradesClass tradesClassTypeTradesClass) =
        object
        [ "additionalProperties" .= additionalPropertiesTradesClass
        , "properties" .= propertiesTradesClass
        , "required" .= requiredTradesClass
        , "title" .= titleTradesClass
        , "type" .= tradesClassTypeTradesClass
        ]

instance FromJSON TradesClass where
    parseJSON (Object v) = TradesClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON TradesProperties where
    toJSON (TradesProperties tradesPropertiesDataTradesProperties statusTradesProperties) =
        object
        [ "data" .= tradesPropertiesDataTradesProperties
        , "status" .= statusTradesProperties
        ]

instance FromJSON TradesProperties where
    parseJSON (Object v) = TradesProperties
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

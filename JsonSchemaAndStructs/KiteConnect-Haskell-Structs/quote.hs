{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( Quote (..)
    , Definitions (..)
    , Buy (..)
    , BuyProperties (..)
    , Orders (..)
    , Data (..)
    , DataProperties (..)
    , NseInfy (..)
    , Depth (..)
    , DepthProperties (..)
    , BuyClass (..)
    , NseInfyClass (..)
    , NseInfyProperties (..)
    , LastTradeTime (..)
    , Ohlc (..)
    , OhlcProperties (..)
    , QuoteClass (..)
    , QuoteProperties (..)
    , Type (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data Quote = Quote
    { refQuote :: Text
    , schemaQuote :: Text
    , definitionsQuote :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { buyDefinitions :: Buy
    , definitionsDataDefinitions :: Data
    , depthDefinitions :: Depth
    , nseInfyDefinitions :: NseInfyClass
    , ohlcDefinitions :: Ohlc
    , quoteDefinitions :: QuoteClass
    } deriving (Show)

data Buy = Buy
    { additionalPropertiesBuy :: Bool
    , propertiesBuy :: BuyProperties
    , requiredBuy :: Vector Text
    , titleBuy :: Text
    , buyTypeBuy :: Text
    } deriving (Show)

data BuyProperties = BuyProperties
    { ordersBuyProperties :: Orders
    , priceBuyProperties :: Orders
    , quantityBuyProperties :: Orders
    } deriving (Show)

data Orders = Orders
    { ordersTypeOrders :: Type
    } deriving (Show)

data Type
    = IntegerType
    | NumberType
    | StringType
    deriving (Show)

data Data = Data
    { additionalPropertiesData :: Bool
    , propertiesData :: DataProperties
    , requiredData :: Vector Text
    , titleData :: Text
    , dataTypeData :: Text
    } deriving (Show)

data DataProperties = DataProperties
    { nseInfyDataProperties :: NseInfy
    } deriving (Show)

data NseInfy = NseInfy
    { refNseInfy :: Text
    } deriving (Show)

data Depth = Depth
    { additionalPropertiesDepth :: Bool
    , propertiesDepth :: DepthProperties
    , requiredDepth :: Vector Text
    , titleDepth :: Text
    , depthTypeDepth :: Text
    } deriving (Show)

data DepthProperties = DepthProperties
    { buyDepthProperties :: BuyClass
    , sellDepthProperties :: BuyClass
    } deriving (Show)

data BuyClass = BuyClass
    { itemsBuyClass :: NseInfy
    , buyClassTypeBuyClass :: Text
    } deriving (Show)

data NseInfyClass = NseInfyClass
    { additionalPropertiesNseInfyClass :: Bool
    , propertiesNseInfyClass :: NseInfyProperties
    , requiredNseInfyClass :: Vector Text
    , titleNseInfyClass :: Text
    , nseInfyClassTypeNseInfyClass :: Text
    } deriving (Show)

data NseInfyProperties = NseInfyProperties
    { averagePriceNseInfyProperties :: Orders
    , buyQuantityNseInfyProperties :: Orders
    , depthNseInfyProperties :: NseInfy
    , instrumentTokenNseInfyProperties :: Orders
    , lastPriceNseInfyProperties :: Orders
    , lastQuantityNseInfyProperties :: Orders
    , lastTradeTimeNseInfyProperties :: LastTradeTime
    , lowerCircuitLimitNseInfyProperties :: Orders
    , netChangeNseInfyProperties :: Orders
    , ohlcNseInfyProperties :: NseInfy
    , oiNseInfyProperties :: Orders
    , oiDayHighNseInfyProperties :: Orders
    , oiDayLowNseInfyProperties :: Orders
    , sellQuantityNseInfyProperties :: Orders
    , timestampNseInfyProperties :: LastTradeTime
    , upperCircuitLimitNseInfyProperties :: Orders
    , volumeNseInfyProperties :: Orders
    } deriving (Show)

data LastTradeTime = LastTradeTime
    { formatLastTradeTime :: Text
    , lastTradeTimeTypeLastTradeTime :: Type
    } deriving (Show)

data Ohlc = Ohlc
    { additionalPropertiesOhlc :: Bool
    , propertiesOhlc :: OhlcProperties
    , requiredOhlc :: Vector Text
    , titleOhlc :: Text
    , ohlcTypeOhlc :: Text
    } deriving (Show)

data OhlcProperties = OhlcProperties
    { closeOhlcProperties :: Orders
    , highOhlcProperties :: Orders
    , lowOhlcProperties :: Orders
    , openOhlcProperties :: Orders
    } deriving (Show)

data QuoteClass = QuoteClass
    { additionalPropertiesQuoteClass :: Bool
    , propertiesQuoteClass :: QuoteProperties
    , requiredQuoteClass :: Vector Text
    , titleQuoteClass :: Text
    , quoteClassTypeQuoteClass :: Text
    } deriving (Show)

data QuoteProperties = QuoteProperties
    { quotePropertiesDataQuoteProperties :: NseInfy
    , statusQuoteProperties :: Orders
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Quote
decodeTopLevel = decode

instance ToJSON Quote where
    toJSON (Quote refQuote schemaQuote definitionsQuote) =
        object
        [ "$ref" .= refQuote
        , "$schema" .= schemaQuote
        , "definitions" .= definitionsQuote
        ]

instance FromJSON Quote where
    parseJSON (Object v) = Quote
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions buyDefinitions definitionsDataDefinitions depthDefinitions nseInfyDefinitions ohlcDefinitions quoteDefinitions) =
        object
        [ "Buy" .= buyDefinitions
        , "Data" .= definitionsDataDefinitions
        , "Depth" .= depthDefinitions
        , "NseInfy" .= nseInfyDefinitions
        , "Ohlc" .= ohlcDefinitions
        , "Quote" .= quoteDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Buy"
        <*> v .: "Data"
        <*> v .: "Depth"
        <*> v .: "NseInfy"
        <*> v .: "Ohlc"
        <*> v .: "Quote"

instance ToJSON Buy where
    toJSON (Buy additionalPropertiesBuy propertiesBuy requiredBuy titleBuy buyTypeBuy) =
        object
        [ "additionalProperties" .= additionalPropertiesBuy
        , "properties" .= propertiesBuy
        , "required" .= requiredBuy
        , "title" .= titleBuy
        , "type" .= buyTypeBuy
        ]

instance FromJSON Buy where
    parseJSON (Object v) = Buy
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON BuyProperties where
    toJSON (BuyProperties ordersBuyProperties priceBuyProperties quantityBuyProperties) =
        object
        [ "orders" .= ordersBuyProperties
        , "price" .= priceBuyProperties
        , "quantity" .= quantityBuyProperties
        ]

instance FromJSON BuyProperties where
    parseJSON (Object v) = BuyProperties
        <$> v .: "orders"
        <*> v .: "price"
        <*> v .: "quantity"

instance ToJSON Orders where
    toJSON (Orders ordersTypeOrders) =
        object
        [ "type" .= ordersTypeOrders
        ]

instance FromJSON Orders where
    parseJSON (Object v) = Orders
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

instance ToJSON Data where
    toJSON (Data additionalPropertiesData propertiesData requiredData titleData dataTypeData) =
        object
        [ "additionalProperties" .= additionalPropertiesData
        , "properties" .= propertiesData
        , "required" .= requiredData
        , "title" .= titleData
        , "type" .= dataTypeData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON DataProperties where
    toJSON (DataProperties nseInfyDataProperties) =
        object
        [ "NSE:INFY" .= nseInfyDataProperties
        ]

instance FromJSON DataProperties where
    parseJSON (Object v) = DataProperties
        <$> v .: "NSE:INFY"

instance ToJSON NseInfy where
    toJSON (NseInfy refNseInfy) =
        object
        [ "$ref" .= refNseInfy
        ]

instance FromJSON NseInfy where
    parseJSON (Object v) = NseInfy
        <$> v .: "$ref"

instance ToJSON Depth where
    toJSON (Depth additionalPropertiesDepth propertiesDepth requiredDepth titleDepth depthTypeDepth) =
        object
        [ "additionalProperties" .= additionalPropertiesDepth
        , "properties" .= propertiesDepth
        , "required" .= requiredDepth
        , "title" .= titleDepth
        , "type" .= depthTypeDepth
        ]

instance FromJSON Depth where
    parseJSON (Object v) = Depth
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON DepthProperties where
    toJSON (DepthProperties buyDepthProperties sellDepthProperties) =
        object
        [ "buy" .= buyDepthProperties
        , "sell" .= sellDepthProperties
        ]

instance FromJSON DepthProperties where
    parseJSON (Object v) = DepthProperties
        <$> v .: "buy"
        <*> v .: "sell"

instance ToJSON BuyClass where
    toJSON (BuyClass itemsBuyClass buyClassTypeBuyClass) =
        object
        [ "items" .= itemsBuyClass
        , "type" .= buyClassTypeBuyClass
        ]

instance FromJSON BuyClass where
    parseJSON (Object v) = BuyClass
        <$> v .: "items"
        <*> v .: "type"

instance ToJSON NseInfyClass where
    toJSON (NseInfyClass additionalPropertiesNseInfyClass propertiesNseInfyClass requiredNseInfyClass titleNseInfyClass nseInfyClassTypeNseInfyClass) =
        object
        [ "additionalProperties" .= additionalPropertiesNseInfyClass
        , "properties" .= propertiesNseInfyClass
        , "required" .= requiredNseInfyClass
        , "title" .= titleNseInfyClass
        , "type" .= nseInfyClassTypeNseInfyClass
        ]

instance FromJSON NseInfyClass where
    parseJSON (Object v) = NseInfyClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON NseInfyProperties where
    toJSON (NseInfyProperties averagePriceNseInfyProperties buyQuantityNseInfyProperties depthNseInfyProperties instrumentTokenNseInfyProperties lastPriceNseInfyProperties lastQuantityNseInfyProperties lastTradeTimeNseInfyProperties lowerCircuitLimitNseInfyProperties netChangeNseInfyProperties ohlcNseInfyProperties oiNseInfyProperties oiDayHighNseInfyProperties oiDayLowNseInfyProperties sellQuantityNseInfyProperties timestampNseInfyProperties upperCircuitLimitNseInfyProperties volumeNseInfyProperties) =
        object
        [ "average_price" .= averagePriceNseInfyProperties
        , "buy_quantity" .= buyQuantityNseInfyProperties
        , "depth" .= depthNseInfyProperties
        , "instrument_token" .= instrumentTokenNseInfyProperties
        , "last_price" .= lastPriceNseInfyProperties
        , "last_quantity" .= lastQuantityNseInfyProperties
        , "last_trade_time" .= lastTradeTimeNseInfyProperties
        , "lower_circuit_limit" .= lowerCircuitLimitNseInfyProperties
        , "net_change" .= netChangeNseInfyProperties
        , "ohlc" .= ohlcNseInfyProperties
        , "oi" .= oiNseInfyProperties
        , "oi_day_high" .= oiDayHighNseInfyProperties
        , "oi_day_low" .= oiDayLowNseInfyProperties
        , "sell_quantity" .= sellQuantityNseInfyProperties
        , "timestamp" .= timestampNseInfyProperties
        , "upper_circuit_limit" .= upperCircuitLimitNseInfyProperties
        , "volume" .= volumeNseInfyProperties
        ]

instance FromJSON NseInfyProperties where
    parseJSON (Object v) = NseInfyProperties
        <$> v .: "average_price"
        <*> v .: "buy_quantity"
        <*> v .: "depth"
        <*> v .: "instrument_token"
        <*> v .: "last_price"
        <*> v .: "last_quantity"
        <*> v .: "last_trade_time"
        <*> v .: "lower_circuit_limit"
        <*> v .: "net_change"
        <*> v .: "ohlc"
        <*> v .: "oi"
        <*> v .: "oi_day_high"
        <*> v .: "oi_day_low"
        <*> v .: "sell_quantity"
        <*> v .: "timestamp"
        <*> v .: "upper_circuit_limit"
        <*> v .: "volume"

instance ToJSON LastTradeTime where
    toJSON (LastTradeTime formatLastTradeTime lastTradeTimeTypeLastTradeTime) =
        object
        [ "format" .= formatLastTradeTime
        , "type" .= lastTradeTimeTypeLastTradeTime
        ]

instance FromJSON LastTradeTime where
    parseJSON (Object v) = LastTradeTime
        <$> v .: "format"
        <*> v .: "type"

instance ToJSON Ohlc where
    toJSON (Ohlc additionalPropertiesOhlc propertiesOhlc requiredOhlc titleOhlc ohlcTypeOhlc) =
        object
        [ "additionalProperties" .= additionalPropertiesOhlc
        , "properties" .= propertiesOhlc
        , "required" .= requiredOhlc
        , "title" .= titleOhlc
        , "type" .= ohlcTypeOhlc
        ]

instance FromJSON Ohlc where
    parseJSON (Object v) = Ohlc
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON OhlcProperties where
    toJSON (OhlcProperties closeOhlcProperties highOhlcProperties lowOhlcProperties openOhlcProperties) =
        object
        [ "close" .= closeOhlcProperties
        , "high" .= highOhlcProperties
        , "low" .= lowOhlcProperties
        , "open" .= openOhlcProperties
        ]

instance FromJSON OhlcProperties where
    parseJSON (Object v) = OhlcProperties
        <$> v .: "close"
        <*> v .: "high"
        <*> v .: "low"
        <*> v .: "open"

instance ToJSON QuoteClass where
    toJSON (QuoteClass additionalPropertiesQuoteClass propertiesQuoteClass requiredQuoteClass titleQuoteClass quoteClassTypeQuoteClass) =
        object
        [ "additionalProperties" .= additionalPropertiesQuoteClass
        , "properties" .= propertiesQuoteClass
        , "required" .= requiredQuoteClass
        , "title" .= titleQuoteClass
        , "type" .= quoteClassTypeQuoteClass
        ]

instance FromJSON QuoteClass where
    parseJSON (Object v) = QuoteClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON QuoteProperties where
    toJSON (QuoteProperties quotePropertiesDataQuoteProperties statusQuoteProperties) =
        object
        [ "data" .= quotePropertiesDataQuoteProperties
        , "status" .= statusQuoteProperties
        ]

instance FromJSON QuoteProperties where
    parseJSON (Object v) = QuoteProperties
        <$> v .: "data"
        <*> v .: "status"

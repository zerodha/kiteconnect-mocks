{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( MFHoldings (..)
    , Definitions (..)
    , Datum (..)
    , DatumProperties (..)
    , AveragePrice (..)
    , MFHoldingsClass (..)
    , MFHoldingsProperties (..)
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

data MFHoldings = MFHoldings
    { refMFHoldings :: Text
    , schemaMFHoldings :: Text
    , definitionsMFHoldings :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { datumDefinitions :: Datum
    , mfHoldingsDefinitions :: MFHoldingsClass
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
    , folioDatumProperties :: AveragePrice
    , fundDatumProperties :: AveragePrice
    , lastPriceDatumProperties :: AveragePrice
    , lastPriceDateDatumProperties :: AveragePrice
    , pledgedQuantityDatumProperties :: AveragePrice
    , pnlDatumProperties :: AveragePrice
    , quantityDatumProperties :: AveragePrice
    , tradingsymbolDatumProperties :: AveragePrice
    } deriving (Show)

data AveragePrice = AveragePrice
    { averagePriceTypeAveragePrice :: Type
    } deriving (Show)

data Type
    = IntegerType
    | NumberType
    | StringType
    deriving (Show)

data MFHoldingsClass = MFHoldingsClass
    { additionalPropertiesMFHoldingsClass :: Bool
    , propertiesMFHoldingsClass :: MFHoldingsProperties
    , requiredMFHoldingsClass :: Vector Text
    , titleMFHoldingsClass :: Text
    , mfHoldingsClassTypeMFHoldingsClass :: Text
    } deriving (Show)

data MFHoldingsProperties = MFHoldingsProperties
    { mfHoldingsPropertiesDataMFHoldingsProperties :: Data
    , statusMFHoldingsProperties :: AveragePrice
    } deriving (Show)

data Data = Data
    { itemsData :: Items
    , dataTypeData :: Text
    } deriving (Show)

data Items = Items
    { refItems :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFHoldings
decodeTopLevel = decode

instance ToJSON MFHoldings where
    toJSON (MFHoldings refMFHoldings schemaMFHoldings definitionsMFHoldings) =
        object
        [ "$ref" .= refMFHoldings
        , "$schema" .= schemaMFHoldings
        , "definitions" .= definitionsMFHoldings
        ]

instance FromJSON MFHoldings where
    parseJSON (Object v) = MFHoldings
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions datumDefinitions mfHoldingsDefinitions) =
        object
        [ "Datum" .= datumDefinitions
        , "MFHoldings" .= mfHoldingsDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Datum"
        <*> v .: "MFHoldings"

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
    toJSON (DatumProperties averagePriceDatumProperties folioDatumProperties fundDatumProperties lastPriceDatumProperties lastPriceDateDatumProperties pledgedQuantityDatumProperties pnlDatumProperties quantityDatumProperties tradingsymbolDatumProperties) =
        object
        [ "average_price" .= averagePriceDatumProperties
        , "folio" .= folioDatumProperties
        , "fund" .= fundDatumProperties
        , "last_price" .= lastPriceDatumProperties
        , "last_price_date" .= lastPriceDateDatumProperties
        , "pledged_quantity" .= pledgedQuantityDatumProperties
        , "pnl" .= pnlDatumProperties
        , "quantity" .= quantityDatumProperties
        , "tradingsymbol" .= tradingsymbolDatumProperties
        ]

instance FromJSON DatumProperties where
    parseJSON (Object v) = DatumProperties
        <$> v .: "average_price"
        <*> v .: "folio"
        <*> v .: "fund"
        <*> v .: "last_price"
        <*> v .: "last_price_date"
        <*> v .: "pledged_quantity"
        <*> v .: "pnl"
        <*> v .: "quantity"
        <*> v .: "tradingsymbol"

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

instance ToJSON MFHoldingsClass where
    toJSON (MFHoldingsClass additionalPropertiesMFHoldingsClass propertiesMFHoldingsClass requiredMFHoldingsClass titleMFHoldingsClass mfHoldingsClassTypeMFHoldingsClass) =
        object
        [ "additionalProperties" .= additionalPropertiesMFHoldingsClass
        , "properties" .= propertiesMFHoldingsClass
        , "required" .= requiredMFHoldingsClass
        , "title" .= titleMFHoldingsClass
        , "type" .= mfHoldingsClassTypeMFHoldingsClass
        ]

instance FromJSON MFHoldingsClass where
    parseJSON (Object v) = MFHoldingsClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON MFHoldingsProperties where
    toJSON (MFHoldingsProperties mfHoldingsPropertiesDataMFHoldingsProperties statusMFHoldingsProperties) =
        object
        [ "data" .= mfHoldingsPropertiesDataMFHoldingsProperties
        , "status" .= statusMFHoldingsProperties
        ]

instance FromJSON MFHoldingsProperties where
    parseJSON (Object v) = MFHoldingsProperties
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

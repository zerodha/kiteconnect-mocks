{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( Holdings (..)
    , Definitions (..)
    , Datum (..)
    , DatumProperties (..)
    , AuthorisedDate (..)
    , AuthorisedQuantity (..)
    , HoldingsClass (..)
    , HoldingsProperties (..)
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

data Holdings = Holdings
    { refHoldings :: Text
    , schemaHoldings :: Text
    , definitionsHoldings :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { datumDefinitions :: Datum
    , holdingsDefinitions :: HoldingsClass
    } deriving (Show)

data Datum = Datum
    { additionalPropertiesDatum :: Bool
    , propertiesDatum :: DatumProperties
    , requiredDatum :: Vector Text
    , titleDatum :: Text
    , datumTypeDatum :: Text
    } deriving (Show)

data DatumProperties = DatumProperties
    { authorisedDateDatumProperties :: AuthorisedDate
    , authorisedQuantityDatumProperties :: AuthorisedQuantity
    , averagePriceDatumProperties :: AuthorisedQuantity
    , closePriceDatumProperties :: AuthorisedQuantity
    , collateralQuantityDatumProperties :: AuthorisedQuantity
    , collateralTypeDatumProperties :: AuthorisedQuantity
    , dayChangeDatumProperties :: AuthorisedQuantity
    , dayChangePercentageDatumProperties :: AuthorisedQuantity
    , discrepancyDatumProperties :: AuthorisedQuantity
    , exchangeDatumProperties :: AuthorisedQuantity
    , instrumentTokenDatumProperties :: AuthorisedQuantity
    , isinDatumProperties :: AuthorisedQuantity
    , lastPriceDatumProperties :: AuthorisedQuantity
    , openingQuantityDatumProperties :: AuthorisedQuantity
    , pnlDatumProperties :: AuthorisedQuantity
    , priceDatumProperties :: AuthorisedQuantity
    , productDatumProperties :: AuthorisedQuantity
    , quantityDatumProperties :: AuthorisedQuantity
    , realisedQuantityDatumProperties :: AuthorisedQuantity
    , t1QuantityDatumProperties :: AuthorisedQuantity
    , tradingsymbolDatumProperties :: AuthorisedQuantity
    , usedQuantityDatumProperties :: AuthorisedQuantity
    } deriving (Show)

data AuthorisedDate = AuthorisedDate
    { formatAuthorisedDate :: Text
    , authorisedDateTypeAuthorisedDate :: Type
    } deriving (Show)

data Type
    = BooleanType
    | IntegerType
    | NumberType
    | StringType
    deriving (Show)

data AuthorisedQuantity = AuthorisedQuantity
    { authorisedQuantityTypeAuthorisedQuantity :: Type
    } deriving (Show)

data HoldingsClass = HoldingsClass
    { additionalPropertiesHoldingsClass :: Bool
    , propertiesHoldingsClass :: HoldingsProperties
    , requiredHoldingsClass :: Vector Text
    , titleHoldingsClass :: Text
    , holdingsClassTypeHoldingsClass :: Text
    } deriving (Show)

data HoldingsProperties = HoldingsProperties
    { holdingsPropertiesDataHoldingsProperties :: Data
    , statusHoldingsProperties :: AuthorisedQuantity
    } deriving (Show)

data Data = Data
    { itemsData :: Items
    , dataTypeData :: Text
    } deriving (Show)

data Items = Items
    { refItems :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Holdings
decodeTopLevel = decode

instance ToJSON Holdings where
    toJSON (Holdings refHoldings schemaHoldings definitionsHoldings) =
        object
        [ "$ref" .= refHoldings
        , "$schema" .= schemaHoldings
        , "definitions" .= definitionsHoldings
        ]

instance FromJSON Holdings where
    parseJSON (Object v) = Holdings
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions datumDefinitions holdingsDefinitions) =
        object
        [ "Datum" .= datumDefinitions
        , "Holdings" .= holdingsDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Datum"
        <*> v .: "Holdings"

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
    toJSON (DatumProperties authorisedDateDatumProperties authorisedQuantityDatumProperties averagePriceDatumProperties closePriceDatumProperties collateralQuantityDatumProperties collateralTypeDatumProperties dayChangeDatumProperties dayChangePercentageDatumProperties discrepancyDatumProperties exchangeDatumProperties instrumentTokenDatumProperties isinDatumProperties lastPriceDatumProperties openingQuantityDatumProperties pnlDatumProperties priceDatumProperties productDatumProperties quantityDatumProperties realisedQuantityDatumProperties t1QuantityDatumProperties tradingsymbolDatumProperties usedQuantityDatumProperties) =
        object
        [ "authorised_date" .= authorisedDateDatumProperties
        , "authorised_quantity" .= authorisedQuantityDatumProperties
        , "average_price" .= averagePriceDatumProperties
        , "close_price" .= closePriceDatumProperties
        , "collateral_quantity" .= collateralQuantityDatumProperties
        , "collateral_type" .= collateralTypeDatumProperties
        , "day_change" .= dayChangeDatumProperties
        , "day_change_percentage" .= dayChangePercentageDatumProperties
        , "discrepancy" .= discrepancyDatumProperties
        , "exchange" .= exchangeDatumProperties
        , "instrument_token" .= instrumentTokenDatumProperties
        , "isin" .= isinDatumProperties
        , "last_price" .= lastPriceDatumProperties
        , "opening_quantity" .= openingQuantityDatumProperties
        , "pnl" .= pnlDatumProperties
        , "price" .= priceDatumProperties
        , "product" .= productDatumProperties
        , "quantity" .= quantityDatumProperties
        , "realised_quantity" .= realisedQuantityDatumProperties
        , "t1_quantity" .= t1QuantityDatumProperties
        , "tradingsymbol" .= tradingsymbolDatumProperties
        , "used_quantity" .= usedQuantityDatumProperties
        ]

instance FromJSON DatumProperties where
    parseJSON (Object v) = DatumProperties
        <$> v .: "authorised_date"
        <*> v .: "authorised_quantity"
        <*> v .: "average_price"
        <*> v .: "close_price"
        <*> v .: "collateral_quantity"
        <*> v .: "collateral_type"
        <*> v .: "day_change"
        <*> v .: "day_change_percentage"
        <*> v .: "discrepancy"
        <*> v .: "exchange"
        <*> v .: "instrument_token"
        <*> v .: "isin"
        <*> v .: "last_price"
        <*> v .: "opening_quantity"
        <*> v .: "pnl"
        <*> v .: "price"
        <*> v .: "product"
        <*> v .: "quantity"
        <*> v .: "realised_quantity"
        <*> v .: "t1_quantity"
        <*> v .: "tradingsymbol"
        <*> v .: "used_quantity"

instance ToJSON AuthorisedDate where
    toJSON (AuthorisedDate formatAuthorisedDate authorisedDateTypeAuthorisedDate) =
        object
        [ "format" .= formatAuthorisedDate
        , "type" .= authorisedDateTypeAuthorisedDate
        ]

instance FromJSON AuthorisedDate where
    parseJSON (Object v) = AuthorisedDate
        <$> v .: "format"
        <*> v .: "type"

instance ToJSON Type where
    toJSON BooleanType = "boolean"
    toJSON IntegerType = "integer"
    toJSON NumberType = "number"
    toJSON StringType = "string"

instance FromJSON Type where
    parseJSON = withText "Type" parseText
        where
            parseText "boolean" = return BooleanType
            parseText "integer" = return IntegerType
            parseText "number" = return NumberType
            parseText "string" = return StringType

instance ToJSON AuthorisedQuantity where
    toJSON (AuthorisedQuantity authorisedQuantityTypeAuthorisedQuantity) =
        object
        [ "type" .= authorisedQuantityTypeAuthorisedQuantity
        ]

instance FromJSON AuthorisedQuantity where
    parseJSON (Object v) = AuthorisedQuantity
        <$> v .: "type"

instance ToJSON HoldingsClass where
    toJSON (HoldingsClass additionalPropertiesHoldingsClass propertiesHoldingsClass requiredHoldingsClass titleHoldingsClass holdingsClassTypeHoldingsClass) =
        object
        [ "additionalProperties" .= additionalPropertiesHoldingsClass
        , "properties" .= propertiesHoldingsClass
        , "required" .= requiredHoldingsClass
        , "title" .= titleHoldingsClass
        , "type" .= holdingsClassTypeHoldingsClass
        ]

instance FromJSON HoldingsClass where
    parseJSON (Object v) = HoldingsClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON HoldingsProperties where
    toJSON (HoldingsProperties holdingsPropertiesDataHoldingsProperties statusHoldingsProperties) =
        object
        [ "data" .= holdingsPropertiesDataHoldingsProperties
        , "status" .= statusHoldingsProperties
        ]

instance FromJSON HoldingsProperties where
    parseJSON (Object v) = HoldingsProperties
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

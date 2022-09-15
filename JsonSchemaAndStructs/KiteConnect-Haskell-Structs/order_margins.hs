{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( OrderMargins (..)
    , Definitions (..)
    , Datum (..)
    , DatumProperties (..)
    , Additional (..)
    , Pnl (..)
    , OrderMarginsClass (..)
    , OrderMarginsProperties (..)
    , Data (..)
    , PnlClass (..)
    , PnlProperties (..)
    , Type (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data OrderMargins = OrderMargins
    { refOrderMargins :: Text
    , schemaOrderMargins :: Text
    , definitionsOrderMargins :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { datumDefinitions :: Datum
    , orderMarginsDefinitions :: OrderMarginsClass
    , pnlDefinitions :: PnlClass
    } deriving (Show)

data Datum = Datum
    { additionalPropertiesDatum :: Bool
    , propertiesDatum :: DatumProperties
    , requiredDatum :: Vector Text
    , titleDatum :: Text
    , datumTypeDatum :: Text
    } deriving (Show)

data DatumProperties = DatumProperties
    { additionalDatumProperties :: Additional
    , boDatumProperties :: Additional
    , cashDatumProperties :: Additional
    , exchangeDatumProperties :: Additional
    , exposureDatumProperties :: Additional
    , optionPremiumDatumProperties :: Additional
    , pnlDatumProperties :: Pnl
    , spanDatumProperties :: Additional
    , totalDatumProperties :: Additional
    , tradingsymbolDatumProperties :: Additional
    , datumPropertiesTypeDatumProperties :: Additional
    , varDatumProperties :: Additional
    } deriving (Show)

data Additional = Additional
    { additionalTypeAdditional :: Type
    } deriving (Show)

data Type
    = IntegerType
    | NumberType
    | StringType
    deriving (Show)

data Pnl = Pnl
    { refPnl :: Text
    } deriving (Show)

data OrderMarginsClass = OrderMarginsClass
    { additionalPropertiesOrderMarginsClass :: Bool
    , propertiesOrderMarginsClass :: OrderMarginsProperties
    , requiredOrderMarginsClass :: Vector Text
    , titleOrderMarginsClass :: Text
    , orderMarginsClassTypeOrderMarginsClass :: Text
    } deriving (Show)

data OrderMarginsProperties = OrderMarginsProperties
    { orderMarginsPropertiesDataOrderMarginsProperties :: Data
    , statusOrderMarginsProperties :: Additional
    } deriving (Show)

data Data = Data
    { itemsData :: Pnl
    , dataTypeData :: Text
    } deriving (Show)

data PnlClass = PnlClass
    { additionalPropertiesPnlClass :: Bool
    , propertiesPnlClass :: PnlProperties
    , requiredPnlClass :: Vector Text
    , titlePnlClass :: Text
    , pnlClassTypePnlClass :: Text
    } deriving (Show)

data PnlProperties = PnlProperties
    { realisedPnlProperties :: Additional
    , unrealisedPnlProperties :: Additional
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe OrderMargins
decodeTopLevel = decode

instance ToJSON OrderMargins where
    toJSON (OrderMargins refOrderMargins schemaOrderMargins definitionsOrderMargins) =
        object
        [ "$ref" .= refOrderMargins
        , "$schema" .= schemaOrderMargins
        , "definitions" .= definitionsOrderMargins
        ]

instance FromJSON OrderMargins where
    parseJSON (Object v) = OrderMargins
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions datumDefinitions orderMarginsDefinitions pnlDefinitions) =
        object
        [ "Datum" .= datumDefinitions
        , "OrderMargins" .= orderMarginsDefinitions
        , "Pnl" .= pnlDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Datum"
        <*> v .: "OrderMargins"
        <*> v .: "Pnl"

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
    toJSON (DatumProperties additionalDatumProperties boDatumProperties cashDatumProperties exchangeDatumProperties exposureDatumProperties optionPremiumDatumProperties pnlDatumProperties spanDatumProperties totalDatumProperties tradingsymbolDatumProperties datumPropertiesTypeDatumProperties varDatumProperties) =
        object
        [ "additional" .= additionalDatumProperties
        , "bo" .= boDatumProperties
        , "cash" .= cashDatumProperties
        , "exchange" .= exchangeDatumProperties
        , "exposure" .= exposureDatumProperties
        , "option_premium" .= optionPremiumDatumProperties
        , "pnl" .= pnlDatumProperties
        , "span" .= spanDatumProperties
        , "total" .= totalDatumProperties
        , "tradingsymbol" .= tradingsymbolDatumProperties
        , "type" .= datumPropertiesTypeDatumProperties
        , "var" .= varDatumProperties
        ]

instance FromJSON DatumProperties where
    parseJSON (Object v) = DatumProperties
        <$> v .: "additional"
        <*> v .: "bo"
        <*> v .: "cash"
        <*> v .: "exchange"
        <*> v .: "exposure"
        <*> v .: "option_premium"
        <*> v .: "pnl"
        <*> v .: "span"
        <*> v .: "total"
        <*> v .: "tradingsymbol"
        <*> v .: "type"
        <*> v .: "var"

instance ToJSON Additional where
    toJSON (Additional additionalTypeAdditional) =
        object
        [ "type" .= additionalTypeAdditional
        ]

instance FromJSON Additional where
    parseJSON (Object v) = Additional
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

instance ToJSON Pnl where
    toJSON (Pnl refPnl) =
        object
        [ "$ref" .= refPnl
        ]

instance FromJSON Pnl where
    parseJSON (Object v) = Pnl
        <$> v .: "$ref"

instance ToJSON OrderMarginsClass where
    toJSON (OrderMarginsClass additionalPropertiesOrderMarginsClass propertiesOrderMarginsClass requiredOrderMarginsClass titleOrderMarginsClass orderMarginsClassTypeOrderMarginsClass) =
        object
        [ "additionalProperties" .= additionalPropertiesOrderMarginsClass
        , "properties" .= propertiesOrderMarginsClass
        , "required" .= requiredOrderMarginsClass
        , "title" .= titleOrderMarginsClass
        , "type" .= orderMarginsClassTypeOrderMarginsClass
        ]

instance FromJSON OrderMarginsClass where
    parseJSON (Object v) = OrderMarginsClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON OrderMarginsProperties where
    toJSON (OrderMarginsProperties orderMarginsPropertiesDataOrderMarginsProperties statusOrderMarginsProperties) =
        object
        [ "data" .= orderMarginsPropertiesDataOrderMarginsProperties
        , "status" .= statusOrderMarginsProperties
        ]

instance FromJSON OrderMarginsProperties where
    parseJSON (Object v) = OrderMarginsProperties
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

instance ToJSON PnlClass where
    toJSON (PnlClass additionalPropertiesPnlClass propertiesPnlClass requiredPnlClass titlePnlClass pnlClassTypePnlClass) =
        object
        [ "additionalProperties" .= additionalPropertiesPnlClass
        , "properties" .= propertiesPnlClass
        , "required" .= requiredPnlClass
        , "title" .= titlePnlClass
        , "type" .= pnlClassTypePnlClass
        ]

instance FromJSON PnlClass where
    parseJSON (Object v) = PnlClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON PnlProperties where
    toJSON (PnlProperties realisedPnlProperties unrealisedPnlProperties) =
        object
        [ "realised" .= realisedPnlProperties
        , "unrealised" .= unrealisedPnlProperties
        ]

instance FromJSON PnlProperties where
    parseJSON (Object v) = PnlProperties
        <$> v .: "realised"
        <*> v .: "unrealised"

{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( BasketMargins (..)
    , Definitions (..)
    , BasketMarginsClass (..)
    , BasketMarginsProperties (..)
    , Data (..)
    , Status (..)
    , DataClass (..)
    , DataProperties (..)
    , Orders (..)
    , Final (..)
    , FinalProperties (..)
    , Pnl (..)
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

data BasketMargins = BasketMargins
    { refBasketMargins :: Text
    , schemaBasketMargins :: Text
    , definitionsBasketMargins :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { basketMarginsDefinitions :: BasketMarginsClass
    , definitionsDataDefinitions :: DataClass
    , finalDefinitions :: Final
    , pnlDefinitions :: Pnl
    } deriving (Show)

data BasketMarginsClass = BasketMarginsClass
    { additionalPropertiesBasketMarginsClass :: Bool
    , propertiesBasketMarginsClass :: BasketMarginsProperties
    , requiredBasketMarginsClass :: Vector Text
    , titleBasketMarginsClass :: Text
    , basketMarginsClassTypeBasketMarginsClass :: Text
    } deriving (Show)

data BasketMarginsProperties = BasketMarginsProperties
    { basketMarginsPropertiesDataBasketMarginsProperties :: Data
    , statusBasketMarginsProperties :: Status
    } deriving (Show)

data Data = Data
    { refData :: Text
    } deriving (Show)

data Status = Status
    { statusTypeStatus :: Type
    } deriving (Show)

data Type
    = IntegerType
    | NumberType
    | StringType
    deriving (Show)

data DataClass = DataClass
    { additionalPropertiesDataClass :: Bool
    , propertiesDataClass :: DataProperties
    , requiredDataClass :: Vector Text
    , titleDataClass :: Text
    , dataClassTypeDataClass :: Text
    } deriving (Show)

data DataProperties = DataProperties
    { finalDataProperties :: Data
    , initialDataProperties :: Data
    , ordersDataProperties :: Orders
    } deriving (Show)

data Orders = Orders
    { itemsOrders :: Data
    , ordersTypeOrders :: Text
    } deriving (Show)

data Final = Final
    { additionalPropertiesFinal :: Bool
    , propertiesFinal :: FinalProperties
    , requiredFinal :: Vector Text
    , titleFinal :: Text
    , finalTypeFinal :: Text
    } deriving (Show)

data FinalProperties = FinalProperties
    { additionalFinalProperties :: Status
    , boFinalProperties :: Status
    , cashFinalProperties :: Status
    , exchangeFinalProperties :: Status
    , exposureFinalProperties :: Status
    , optionPremiumFinalProperties :: Status
    , pnlFinalProperties :: Data
    , spanFinalProperties :: Status
    , totalFinalProperties :: Status
    , tradingsymbolFinalProperties :: Status
    , finalPropertiesTypeFinalProperties :: Status
    , varFinalProperties :: Status
    } deriving (Show)

data Pnl = Pnl
    { additionalPropertiesPnl :: Bool
    , propertiesPnl :: PnlProperties
    , requiredPnl :: Vector Text
    , titlePnl :: Text
    , pnlTypePnl :: Text
    } deriving (Show)

data PnlProperties = PnlProperties
    { realisedPnlProperties :: Status
    , unrealisedPnlProperties :: Status
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe BasketMargins
decodeTopLevel = decode

instance ToJSON BasketMargins where
    toJSON (BasketMargins refBasketMargins schemaBasketMargins definitionsBasketMargins) =
        object
        [ "$ref" .= refBasketMargins
        , "$schema" .= schemaBasketMargins
        , "definitions" .= definitionsBasketMargins
        ]

instance FromJSON BasketMargins where
    parseJSON (Object v) = BasketMargins
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions basketMarginsDefinitions definitionsDataDefinitions finalDefinitions pnlDefinitions) =
        object
        [ "BasketMargins" .= basketMarginsDefinitions
        , "Data" .= definitionsDataDefinitions
        , "Final" .= finalDefinitions
        , "Pnl" .= pnlDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "BasketMargins"
        <*> v .: "Data"
        <*> v .: "Final"
        <*> v .: "Pnl"

instance ToJSON BasketMarginsClass where
    toJSON (BasketMarginsClass additionalPropertiesBasketMarginsClass propertiesBasketMarginsClass requiredBasketMarginsClass titleBasketMarginsClass basketMarginsClassTypeBasketMarginsClass) =
        object
        [ "additionalProperties" .= additionalPropertiesBasketMarginsClass
        , "properties" .= propertiesBasketMarginsClass
        , "required" .= requiredBasketMarginsClass
        , "title" .= titleBasketMarginsClass
        , "type" .= basketMarginsClassTypeBasketMarginsClass
        ]

instance FromJSON BasketMarginsClass where
    parseJSON (Object v) = BasketMarginsClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON BasketMarginsProperties where
    toJSON (BasketMarginsProperties basketMarginsPropertiesDataBasketMarginsProperties statusBasketMarginsProperties) =
        object
        [ "data" .= basketMarginsPropertiesDataBasketMarginsProperties
        , "status" .= statusBasketMarginsProperties
        ]

instance FromJSON BasketMarginsProperties where
    parseJSON (Object v) = BasketMarginsProperties
        <$> v .: "data"
        <*> v .: "status"

instance ToJSON Data where
    toJSON (Data refData) =
        object
        [ "$ref" .= refData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .: "$ref"

instance ToJSON Status where
    toJSON (Status statusTypeStatus) =
        object
        [ "type" .= statusTypeStatus
        ]

instance FromJSON Status where
    parseJSON (Object v) = Status
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

instance ToJSON DataClass where
    toJSON (DataClass additionalPropertiesDataClass propertiesDataClass requiredDataClass titleDataClass dataClassTypeDataClass) =
        object
        [ "additionalProperties" .= additionalPropertiesDataClass
        , "properties" .= propertiesDataClass
        , "required" .= requiredDataClass
        , "title" .= titleDataClass
        , "type" .= dataClassTypeDataClass
        ]

instance FromJSON DataClass where
    parseJSON (Object v) = DataClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON DataProperties where
    toJSON (DataProperties finalDataProperties initialDataProperties ordersDataProperties) =
        object
        [ "final" .= finalDataProperties
        , "initial" .= initialDataProperties
        , "orders" .= ordersDataProperties
        ]

instance FromJSON DataProperties where
    parseJSON (Object v) = DataProperties
        <$> v .: "final"
        <*> v .: "initial"
        <*> v .: "orders"

instance ToJSON Orders where
    toJSON (Orders itemsOrders ordersTypeOrders) =
        object
        [ "items" .= itemsOrders
        , "type" .= ordersTypeOrders
        ]

instance FromJSON Orders where
    parseJSON (Object v) = Orders
        <$> v .: "items"
        <*> v .: "type"

instance ToJSON Final where
    toJSON (Final additionalPropertiesFinal propertiesFinal requiredFinal titleFinal finalTypeFinal) =
        object
        [ "additionalProperties" .= additionalPropertiesFinal
        , "properties" .= propertiesFinal
        , "required" .= requiredFinal
        , "title" .= titleFinal
        , "type" .= finalTypeFinal
        ]

instance FromJSON Final where
    parseJSON (Object v) = Final
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON FinalProperties where
    toJSON (FinalProperties additionalFinalProperties boFinalProperties cashFinalProperties exchangeFinalProperties exposureFinalProperties optionPremiumFinalProperties pnlFinalProperties spanFinalProperties totalFinalProperties tradingsymbolFinalProperties finalPropertiesTypeFinalProperties varFinalProperties) =
        object
        [ "additional" .= additionalFinalProperties
        , "bo" .= boFinalProperties
        , "cash" .= cashFinalProperties
        , "exchange" .= exchangeFinalProperties
        , "exposure" .= exposureFinalProperties
        , "option_premium" .= optionPremiumFinalProperties
        , "pnl" .= pnlFinalProperties
        , "span" .= spanFinalProperties
        , "total" .= totalFinalProperties
        , "tradingsymbol" .= tradingsymbolFinalProperties
        , "type" .= finalPropertiesTypeFinalProperties
        , "var" .= varFinalProperties
        ]

instance FromJSON FinalProperties where
    parseJSON (Object v) = FinalProperties
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

instance ToJSON Pnl where
    toJSON (Pnl additionalPropertiesPnl propertiesPnl requiredPnl titlePnl pnlTypePnl) =
        object
        [ "additionalProperties" .= additionalPropertiesPnl
        , "properties" .= propertiesPnl
        , "required" .= requiredPnl
        , "title" .= titlePnl
        , "type" .= pnlTypePnl
        ]

instance FromJSON Pnl where
    parseJSON (Object v) = Pnl
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

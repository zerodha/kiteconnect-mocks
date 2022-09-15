{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( MFSips (..)
    , Definitions (..)
    , Datum (..)
    , DatumProperties (..)
    , CompletedInstalments (..)
    , Created (..)
    , SIPRegNum (..)
    , StepUp (..)
    , MFSipsClass (..)
    , MFSipsProperties (..)
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

data MFSips = MFSips
    { refMFSips :: Text
    , schemaMFSips :: Text
    , definitionsMFSips :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { datumDefinitions :: Datum
    , mfSipsDefinitions :: MFSipsClass
    } deriving (Show)

data Datum = Datum
    { additionalPropertiesDatum :: Bool
    , propertiesDatum :: DatumProperties
    , requiredDatum :: Vector Text
    , titleDatum :: Text
    , datumTypeDatum :: Text
    } deriving (Show)

data DatumProperties = DatumProperties
    { completedInstalmentsDatumProperties :: CompletedInstalments
    , createdDatumProperties :: Created
    , dividendTypeDatumProperties :: CompletedInstalments
    , frequencyDatumProperties :: CompletedInstalments
    , fundDatumProperties :: CompletedInstalments
    , instalmentAmountDatumProperties :: CompletedInstalments
    , instalmentDayDatumProperties :: CompletedInstalments
    , instalmentsDatumProperties :: CompletedInstalments
    , lastInstalmentDatumProperties :: Created
    , nextInstalmentDatumProperties :: Created
    , pendingInstalmentsDatumProperties :: CompletedInstalments
    , sipIDDatumProperties :: CompletedInstalments
    , sipRegNumDatumProperties :: SIPRegNum
    , sipTypeDatumProperties :: CompletedInstalments
    , statusDatumProperties :: CompletedInstalments
    , stepUpDatumProperties :: StepUp
    , tagDatumProperties :: CompletedInstalments
    , tradingsymbolDatumProperties :: CompletedInstalments
    , transactionTypeDatumProperties :: CompletedInstalments
    , triggerPriceDatumProperties :: CompletedInstalments
    } deriving (Show)

data CompletedInstalments = CompletedInstalments
    { completedInstalmentsTypeCompletedInstalments :: Type
    } deriving (Show)

data Type
    = IntegerType
    | NumberType
    | StringType
    deriving (Show)

data Created = Created
    { formatCreated :: Maybe Text
    , createdTypeCreated :: Text
    } deriving (Show)

data SIPRegNum = SIPRegNum
    { anyOfSIPRegNum :: Vector Created
    } deriving (Show)

data StepUp = StepUp
    { additionalPropertiesStepUp :: CompletedInstalments
    , stepUpTypeStepUp :: Text
    } deriving (Show)

data MFSipsClass = MFSipsClass
    { additionalPropertiesMFSipsClass :: Bool
    , propertiesMFSipsClass :: MFSipsProperties
    , requiredMFSipsClass :: Vector Text
    , titleMFSipsClass :: Text
    , mfSipsClassTypeMFSipsClass :: Text
    } deriving (Show)

data MFSipsProperties = MFSipsProperties
    { mfSipsPropertiesDataMFSipsProperties :: Data
    } deriving (Show)

data Data = Data
    { itemsData :: Items
    , dataTypeData :: Text
    } deriving (Show)

data Items = Items
    { refItems :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFSips
decodeTopLevel = decode

instance ToJSON MFSips where
    toJSON (MFSips refMFSips schemaMFSips definitionsMFSips) =
        object
        [ "$ref" .= refMFSips
        , "$schema" .= schemaMFSips
        , "definitions" .= definitionsMFSips
        ]

instance FromJSON MFSips where
    parseJSON (Object v) = MFSips
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions datumDefinitions mfSipsDefinitions) =
        object
        [ "Datum" .= datumDefinitions
        , "MFSips" .= mfSipsDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Datum"
        <*> v .: "MFSips"

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
    toJSON (DatumProperties completedInstalmentsDatumProperties createdDatumProperties dividendTypeDatumProperties frequencyDatumProperties fundDatumProperties instalmentAmountDatumProperties instalmentDayDatumProperties instalmentsDatumProperties lastInstalmentDatumProperties nextInstalmentDatumProperties pendingInstalmentsDatumProperties sipIDDatumProperties sipRegNumDatumProperties sipTypeDatumProperties statusDatumProperties stepUpDatumProperties tagDatumProperties tradingsymbolDatumProperties transactionTypeDatumProperties triggerPriceDatumProperties) =
        object
        [ "completed_instalments" .= completedInstalmentsDatumProperties
        , "created" .= createdDatumProperties
        , "dividend_type" .= dividendTypeDatumProperties
        , "frequency" .= frequencyDatumProperties
        , "fund" .= fundDatumProperties
        , "instalment_amount" .= instalmentAmountDatumProperties
        , "instalment_day" .= instalmentDayDatumProperties
        , "instalments" .= instalmentsDatumProperties
        , "last_instalment" .= lastInstalmentDatumProperties
        , "next_instalment" .= nextInstalmentDatumProperties
        , "pending_instalments" .= pendingInstalmentsDatumProperties
        , "sip_id" .= sipIDDatumProperties
        , "sip_reg_num" .= sipRegNumDatumProperties
        , "sip_type" .= sipTypeDatumProperties
        , "status" .= statusDatumProperties
        , "step_up" .= stepUpDatumProperties
        , "tag" .= tagDatumProperties
        , "tradingsymbol" .= tradingsymbolDatumProperties
        , "transaction_type" .= transactionTypeDatumProperties
        , "trigger_price" .= triggerPriceDatumProperties
        ]

instance FromJSON DatumProperties where
    parseJSON (Object v) = DatumProperties
        <$> v .: "completed_instalments"
        <*> v .: "created"
        <*> v .: "dividend_type"
        <*> v .: "frequency"
        <*> v .: "fund"
        <*> v .: "instalment_amount"
        <*> v .: "instalment_day"
        <*> v .: "instalments"
        <*> v .: "last_instalment"
        <*> v .: "next_instalment"
        <*> v .: "pending_instalments"
        <*> v .: "sip_id"
        <*> v .: "sip_reg_num"
        <*> v .: "sip_type"
        <*> v .: "status"
        <*> v .: "step_up"
        <*> v .: "tag"
        <*> v .: "tradingsymbol"
        <*> v .: "transaction_type"
        <*> v .: "trigger_price"

instance ToJSON CompletedInstalments where
    toJSON (CompletedInstalments completedInstalmentsTypeCompletedInstalments) =
        object
        [ "type" .= completedInstalmentsTypeCompletedInstalments
        ]

instance FromJSON CompletedInstalments where
    parseJSON (Object v) = CompletedInstalments
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

instance ToJSON Created where
    toJSON (Created formatCreated createdTypeCreated) =
        object
        [ "format" .= formatCreated
        , "type" .= createdTypeCreated
        ]

instance FromJSON Created where
    parseJSON (Object v) = Created
        <$> v .:? "format"
        <*> v .: "type"

instance ToJSON SIPRegNum where
    toJSON (SIPRegNum anyOfSIPRegNum) =
        object
        [ "anyOf" .= anyOfSIPRegNum
        ]

instance FromJSON SIPRegNum where
    parseJSON (Object v) = SIPRegNum
        <$> v .: "anyOf"

instance ToJSON StepUp where
    toJSON (StepUp additionalPropertiesStepUp stepUpTypeStepUp) =
        object
        [ "additionalProperties" .= additionalPropertiesStepUp
        , "type" .= stepUpTypeStepUp
        ]

instance FromJSON StepUp where
    parseJSON (Object v) = StepUp
        <$> v .: "additionalProperties"
        <*> v .: "type"

instance ToJSON MFSipsClass where
    toJSON (MFSipsClass additionalPropertiesMFSipsClass propertiesMFSipsClass requiredMFSipsClass titleMFSipsClass mfSipsClassTypeMFSipsClass) =
        object
        [ "additionalProperties" .= additionalPropertiesMFSipsClass
        , "properties" .= propertiesMFSipsClass
        , "required" .= requiredMFSipsClass
        , "title" .= titleMFSipsClass
        , "type" .= mfSipsClassTypeMFSipsClass
        ]

instance FromJSON MFSipsClass where
    parseJSON (Object v) = MFSipsClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON MFSipsProperties where
    toJSON (MFSipsProperties mfSipsPropertiesDataMFSipsProperties) =
        object
        [ "data" .= mfSipsPropertiesDataMFSipsProperties
        ]

instance FromJSON MFSipsProperties where
    parseJSON (Object v) = MFSipsProperties
        <$> v .: "data"

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

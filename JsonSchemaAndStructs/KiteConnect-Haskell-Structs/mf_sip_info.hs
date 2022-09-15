{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( MFSIPInfo (..)
    , Definitions (..)
    , Data (..)
    , DataProperties (..)
    , CompletedInstalments (..)
    , Created (..)
    , StepUp (..)
    , MFSIPInfoClass (..)
    , MFSIPInfoProperties (..)
    , StepUpClass (..)
    , StepUpProperties (..)
    , Type (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data MFSIPInfo = MFSIPInfo
    { refMFSIPInfo :: Text
    , schemaMFSIPInfo :: Text
    , definitionsMFSIPInfo :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { definitionsDataDefinitions :: Data
    , mfsipInfoDefinitions :: MFSIPInfoClass
    , stepUpDefinitions :: StepUpClass
    } deriving (Show)

data Data = Data
    { additionalPropertiesData :: Bool
    , propertiesData :: DataProperties
    , requiredData :: Vector Text
    , titleData :: Text
    , dataTypeData :: Text
    } deriving (Show)

data DataProperties = DataProperties
    { completedInstalmentsDataProperties :: CompletedInstalments
    , createdDataProperties :: Created
    , dividendTypeDataProperties :: CompletedInstalments
    , frequencyDataProperties :: CompletedInstalments
    , fundDataProperties :: CompletedInstalments
    , fundSourceDataProperties :: CompletedInstalments
    , instalmentAmountDataProperties :: CompletedInstalments
    , instalmentDayDataProperties :: CompletedInstalments
    , instalmentsDataProperties :: CompletedInstalments
    , lastInstalmentDataProperties :: Created
    , nextInstalmentDataProperties :: Created
    , pendingInstalmentsDataProperties :: CompletedInstalments
    , sipIDDataProperties :: CompletedInstalments
    , sipRegNumDataProperties :: CompletedInstalments
    , sipTypeDataProperties :: CompletedInstalments
    , statusDataProperties :: CompletedInstalments
    , stepUpDataProperties :: StepUp
    , tagDataProperties :: CompletedInstalments
    , tradingsymbolDataProperties :: CompletedInstalments
    , transactionTypeDataProperties :: CompletedInstalments
    , triggerPriceDataProperties :: CompletedInstalments
    } deriving (Show)

data CompletedInstalments = CompletedInstalments
    { completedInstalmentsTypeCompletedInstalments :: Type
    } deriving (Show)

data Type
    = IntegerType
    | NullType
    | NumberType
    | StringType
    deriving (Show)

data Created = Created
    { formatCreated :: Text
    , createdTypeCreated :: Type
    } deriving (Show)

data StepUp = StepUp
    { refStepUp :: Text
    } deriving (Show)

data MFSIPInfoClass = MFSIPInfoClass
    { additionalPropertiesMFSIPInfoClass :: Bool
    , propertiesMFSIPInfoClass :: MFSIPInfoProperties
    , requiredMFSIPInfoClass :: Vector Text
    , titleMFSIPInfoClass :: Text
    , mfsipInfoClassTypeMFSIPInfoClass :: Text
    } deriving (Show)

data MFSIPInfoProperties = MFSIPInfoProperties
    { mfsipInfoPropertiesDataMFSIPInfoProperties :: StepUp
    , statusMFSIPInfoProperties :: CompletedInstalments
    } deriving (Show)

data StepUpClass = StepUpClass
    { additionalPropertiesStepUpClass :: Bool
    , propertiesStepUpClass :: StepUpProperties
    , requiredStepUpClass :: Vector Text
    , titleStepUpClass :: Text
    , stepUpClassTypeStepUpClass :: Text
    } deriving (Show)

data StepUpProperties = StepUpProperties
    { the1502StepUpProperties :: CompletedInstalments
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFSIPInfo
decodeTopLevel = decode

instance ToJSON MFSIPInfo where
    toJSON (MFSIPInfo refMFSIPInfo schemaMFSIPInfo definitionsMFSIPInfo) =
        object
        [ "$ref" .= refMFSIPInfo
        , "$schema" .= schemaMFSIPInfo
        , "definitions" .= definitionsMFSIPInfo
        ]

instance FromJSON MFSIPInfo where
    parseJSON (Object v) = MFSIPInfo
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions definitionsDataDefinitions mfsipInfoDefinitions stepUpDefinitions) =
        object
        [ "Data" .= definitionsDataDefinitions
        , "MFSIPInfo" .= mfsipInfoDefinitions
        , "StepUp" .= stepUpDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Data"
        <*> v .: "MFSIPInfo"
        <*> v .: "StepUp"

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
    toJSON (DataProperties completedInstalmentsDataProperties createdDataProperties dividendTypeDataProperties frequencyDataProperties fundDataProperties fundSourceDataProperties instalmentAmountDataProperties instalmentDayDataProperties instalmentsDataProperties lastInstalmentDataProperties nextInstalmentDataProperties pendingInstalmentsDataProperties sipIDDataProperties sipRegNumDataProperties sipTypeDataProperties statusDataProperties stepUpDataProperties tagDataProperties tradingsymbolDataProperties transactionTypeDataProperties triggerPriceDataProperties) =
        object
        [ "completed_instalments" .= completedInstalmentsDataProperties
        , "created" .= createdDataProperties
        , "dividend_type" .= dividendTypeDataProperties
        , "frequency" .= frequencyDataProperties
        , "fund" .= fundDataProperties
        , "fund_source" .= fundSourceDataProperties
        , "instalment_amount" .= instalmentAmountDataProperties
        , "instalment_day" .= instalmentDayDataProperties
        , "instalments" .= instalmentsDataProperties
        , "last_instalment" .= lastInstalmentDataProperties
        , "next_instalment" .= nextInstalmentDataProperties
        , "pending_instalments" .= pendingInstalmentsDataProperties
        , "sip_id" .= sipIDDataProperties
        , "sip_reg_num" .= sipRegNumDataProperties
        , "sip_type" .= sipTypeDataProperties
        , "status" .= statusDataProperties
        , "step_up" .= stepUpDataProperties
        , "tag" .= tagDataProperties
        , "tradingsymbol" .= tradingsymbolDataProperties
        , "transaction_type" .= transactionTypeDataProperties
        , "trigger_price" .= triggerPriceDataProperties
        ]

instance FromJSON DataProperties where
    parseJSON (Object v) = DataProperties
        <$> v .: "completed_instalments"
        <*> v .: "created"
        <*> v .: "dividend_type"
        <*> v .: "frequency"
        <*> v .: "fund"
        <*> v .: "fund_source"
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
    toJSON NullType = "null"
    toJSON NumberType = "number"
    toJSON StringType = "string"

instance FromJSON Type where
    parseJSON = withText "Type" parseText
        where
            parseText "integer" = return IntegerType
            parseText "null" = return NullType
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
        <$> v .: "format"
        <*> v .: "type"

instance ToJSON StepUp where
    toJSON (StepUp refStepUp) =
        object
        [ "$ref" .= refStepUp
        ]

instance FromJSON StepUp where
    parseJSON (Object v) = StepUp
        <$> v .: "$ref"

instance ToJSON MFSIPInfoClass where
    toJSON (MFSIPInfoClass additionalPropertiesMFSIPInfoClass propertiesMFSIPInfoClass requiredMFSIPInfoClass titleMFSIPInfoClass mfsipInfoClassTypeMFSIPInfoClass) =
        object
        [ "additionalProperties" .= additionalPropertiesMFSIPInfoClass
        , "properties" .= propertiesMFSIPInfoClass
        , "required" .= requiredMFSIPInfoClass
        , "title" .= titleMFSIPInfoClass
        , "type" .= mfsipInfoClassTypeMFSIPInfoClass
        ]

instance FromJSON MFSIPInfoClass where
    parseJSON (Object v) = MFSIPInfoClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON MFSIPInfoProperties where
    toJSON (MFSIPInfoProperties mfsipInfoPropertiesDataMFSIPInfoProperties statusMFSIPInfoProperties) =
        object
        [ "data" .= mfsipInfoPropertiesDataMFSIPInfoProperties
        , "status" .= statusMFSIPInfoProperties
        ]

instance FromJSON MFSIPInfoProperties where
    parseJSON (Object v) = MFSIPInfoProperties
        <$> v .: "data"
        <*> v .: "status"

instance ToJSON StepUpClass where
    toJSON (StepUpClass additionalPropertiesStepUpClass propertiesStepUpClass requiredStepUpClass titleStepUpClass stepUpClassTypeStepUpClass) =
        object
        [ "additionalProperties" .= additionalPropertiesStepUpClass
        , "properties" .= propertiesStepUpClass
        , "required" .= requiredStepUpClass
        , "title" .= titleStepUpClass
        , "type" .= stepUpClassTypeStepUpClass
        ]

instance FromJSON StepUpClass where
    parseJSON (Object v) = StepUpClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON StepUpProperties where
    toJSON (StepUpProperties the1502StepUpProperties) =
        object
        [ "15-02" .= the1502StepUpProperties
        ]

instance FromJSON StepUpProperties where
    parseJSON (Object v) = StepUpProperties
        <$> v .: "15-02"

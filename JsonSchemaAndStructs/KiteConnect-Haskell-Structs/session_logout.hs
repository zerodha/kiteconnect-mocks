{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( SessionLogout (..)
    , Definitions (..)
    , SessionLogoutClass (..)
    , Properties (..)
    , Data (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data SessionLogout = SessionLogout
    { refSessionLogout :: Text
    , schemaSessionLogout :: Text
    , definitionsSessionLogout :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { sessionLogoutDefinitions :: SessionLogoutClass
    } deriving (Show)

data SessionLogoutClass = SessionLogoutClass
    { additionalPropertiesSessionLogoutClass :: Bool
    , propertiesSessionLogoutClass :: Properties
    , requiredSessionLogoutClass :: Vector Text
    , titleSessionLogoutClass :: Text
    , sessionLogoutClassTypeSessionLogoutClass :: Text
    } deriving (Show)

data Properties = Properties
    { propertiesDataProperties :: Data
    , statusProperties :: Data
    } deriving (Show)

data Data = Data
    { dataTypeData :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe SessionLogout
decodeTopLevel = decode

instance ToJSON SessionLogout where
    toJSON (SessionLogout refSessionLogout schemaSessionLogout definitionsSessionLogout) =
        object
        [ "$ref" .= refSessionLogout
        , "$schema" .= schemaSessionLogout
        , "definitions" .= definitionsSessionLogout
        ]

instance FromJSON SessionLogout where
    parseJSON (Object v) = SessionLogout
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions sessionLogoutDefinitions) =
        object
        [ "SessionLogout" .= sessionLogoutDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "SessionLogout"

instance ToJSON SessionLogoutClass where
    toJSON (SessionLogoutClass additionalPropertiesSessionLogoutClass propertiesSessionLogoutClass requiredSessionLogoutClass titleSessionLogoutClass sessionLogoutClassTypeSessionLogoutClass) =
        object
        [ "additionalProperties" .= additionalPropertiesSessionLogoutClass
        , "properties" .= propertiesSessionLogoutClass
        , "required" .= requiredSessionLogoutClass
        , "title" .= titleSessionLogoutClass
        , "type" .= sessionLogoutClassTypeSessionLogoutClass
        ]

instance FromJSON SessionLogoutClass where
    parseJSON (Object v) = SessionLogoutClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON Properties where
    toJSON (Properties propertiesDataProperties statusProperties) =
        object
        [ "data" .= propertiesDataProperties
        , "status" .= statusProperties
        ]

instance FromJSON Properties where
    parseJSON (Object v) = Properties
        <$> v .: "data"
        <*> v .: "status"

instance ToJSON Data where
    toJSON (Data dataTypeData) =
        object
        [ "type" .= dataTypeData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .: "type"

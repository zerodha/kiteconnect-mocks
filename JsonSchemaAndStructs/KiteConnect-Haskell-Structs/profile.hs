{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( Profile (..)
    , Definitions (..)
    , Data (..)
    , DataProperties (..)
    , AvatarURL (..)
    , Exchanges (..)
    , Meta (..)
    , MetaClass (..)
    , MetaProperties (..)
    , ProfileClass (..)
    , ProfileProperties (..)
    , Type (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data Profile = Profile
    { refProfile :: Text
    , schemaProfile :: Text
    , definitionsProfile :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { definitionsDataDefinitions :: Data
    , metaDefinitions :: MetaClass
    , profileDefinitions :: ProfileClass
    } deriving (Show)

data Data = Data
    { additionalPropertiesData :: Bool
    , propertiesData :: DataProperties
    , requiredData :: Vector Text
    , titleData :: Text
    , dataTypeData :: Text
    } deriving (Show)

data DataProperties = DataProperties
    { avatarURLDataProperties :: AvatarURL
    , brokerDataProperties :: AvatarURL
    , emailDataProperties :: AvatarURL
    , exchangesDataProperties :: Exchanges
    , metaDataProperties :: Meta
    , orderTypesDataProperties :: Exchanges
    , productsDataProperties :: Exchanges
    , userIDDataProperties :: AvatarURL
    , userNameDataProperties :: AvatarURL
    , userShortnameDataProperties :: AvatarURL
    , userTypeDataProperties :: AvatarURL
    } deriving (Show)

data AvatarURL = AvatarURL
    { avatarURLTypeAvatarURL :: Type
    } deriving (Show)

data Type
    = NullType
    | StringType
    deriving (Show)

data Exchanges = Exchanges
    { itemsExchanges :: AvatarURL
    , exchangesTypeExchanges :: Text
    } deriving (Show)

data Meta = Meta
    { refMeta :: Text
    } deriving (Show)

data MetaClass = MetaClass
    { additionalPropertiesMetaClass :: Bool
    , propertiesMetaClass :: MetaProperties
    , requiredMetaClass :: Vector Text
    , titleMetaClass :: Text
    , metaClassTypeMetaClass :: Text
    } deriving (Show)

data MetaProperties = MetaProperties
    { dematConsentMetaProperties :: AvatarURL
    } deriving (Show)

data ProfileClass = ProfileClass
    { additionalPropertiesProfileClass :: Bool
    , propertiesProfileClass :: ProfileProperties
    , requiredProfileClass :: Vector Text
    , titleProfileClass :: Text
    , profileClassTypeProfileClass :: Text
    } deriving (Show)

data ProfileProperties = ProfileProperties
    { profilePropertiesDataProfileProperties :: Meta
    , statusProfileProperties :: AvatarURL
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Profile
decodeTopLevel = decode

instance ToJSON Profile where
    toJSON (Profile refProfile schemaProfile definitionsProfile) =
        object
        [ "$ref" .= refProfile
        , "$schema" .= schemaProfile
        , "definitions" .= definitionsProfile
        ]

instance FromJSON Profile where
    parseJSON (Object v) = Profile
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions definitionsDataDefinitions metaDefinitions profileDefinitions) =
        object
        [ "Data" .= definitionsDataDefinitions
        , "Meta" .= metaDefinitions
        , "Profile" .= profileDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Data"
        <*> v .: "Meta"
        <*> v .: "Profile"

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
    toJSON (DataProperties avatarURLDataProperties brokerDataProperties emailDataProperties exchangesDataProperties metaDataProperties orderTypesDataProperties productsDataProperties userIDDataProperties userNameDataProperties userShortnameDataProperties userTypeDataProperties) =
        object
        [ "avatar_url" .= avatarURLDataProperties
        , "broker" .= brokerDataProperties
        , "email" .= emailDataProperties
        , "exchanges" .= exchangesDataProperties
        , "meta" .= metaDataProperties
        , "order_types" .= orderTypesDataProperties
        , "products" .= productsDataProperties
        , "user_id" .= userIDDataProperties
        , "user_name" .= userNameDataProperties
        , "user_shortname" .= userShortnameDataProperties
        , "user_type" .= userTypeDataProperties
        ]

instance FromJSON DataProperties where
    parseJSON (Object v) = DataProperties
        <$> v .: "avatar_url"
        <*> v .: "broker"
        <*> v .: "email"
        <*> v .: "exchanges"
        <*> v .: "meta"
        <*> v .: "order_types"
        <*> v .: "products"
        <*> v .: "user_id"
        <*> v .: "user_name"
        <*> v .: "user_shortname"
        <*> v .: "user_type"

instance ToJSON AvatarURL where
    toJSON (AvatarURL avatarURLTypeAvatarURL) =
        object
        [ "type" .= avatarURLTypeAvatarURL
        ]

instance FromJSON AvatarURL where
    parseJSON (Object v) = AvatarURL
        <$> v .: "type"

instance ToJSON Type where
    toJSON NullType = "null"
    toJSON StringType = "string"

instance FromJSON Type where
    parseJSON = withText "Type" parseText
        where
            parseText "null" = return NullType
            parseText "string" = return StringType

instance ToJSON Exchanges where
    toJSON (Exchanges itemsExchanges exchangesTypeExchanges) =
        object
        [ "items" .= itemsExchanges
        , "type" .= exchangesTypeExchanges
        ]

instance FromJSON Exchanges where
    parseJSON (Object v) = Exchanges
        <$> v .: "items"
        <*> v .: "type"

instance ToJSON Meta where
    toJSON (Meta refMeta) =
        object
        [ "$ref" .= refMeta
        ]

instance FromJSON Meta where
    parseJSON (Object v) = Meta
        <$> v .: "$ref"

instance ToJSON MetaClass where
    toJSON (MetaClass additionalPropertiesMetaClass propertiesMetaClass requiredMetaClass titleMetaClass metaClassTypeMetaClass) =
        object
        [ "additionalProperties" .= additionalPropertiesMetaClass
        , "properties" .= propertiesMetaClass
        , "required" .= requiredMetaClass
        , "title" .= titleMetaClass
        , "type" .= metaClassTypeMetaClass
        ]

instance FromJSON MetaClass where
    parseJSON (Object v) = MetaClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON MetaProperties where
    toJSON (MetaProperties dematConsentMetaProperties) =
        object
        [ "demat_consent" .= dematConsentMetaProperties
        ]

instance FromJSON MetaProperties where
    parseJSON (Object v) = MetaProperties
        <$> v .: "demat_consent"

instance ToJSON ProfileClass where
    toJSON (ProfileClass additionalPropertiesProfileClass propertiesProfileClass requiredProfileClass titleProfileClass profileClassTypeProfileClass) =
        object
        [ "additionalProperties" .= additionalPropertiesProfileClass
        , "properties" .= propertiesProfileClass
        , "required" .= requiredProfileClass
        , "title" .= titleProfileClass
        , "type" .= profileClassTypeProfileClass
        ]

instance FromJSON ProfileClass where
    parseJSON (Object v) = ProfileClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON ProfileProperties where
    toJSON (ProfileProperties profilePropertiesDataProfileProperties statusProfileProperties) =
        object
        [ "data" .= profilePropertiesDataProfileProperties
        , "status" .= statusProfileProperties
        ]

instance FromJSON ProfileProperties where
    parseJSON (Object v) = ProfileProperties
        <$> v .: "data"
        <*> v .: "status"

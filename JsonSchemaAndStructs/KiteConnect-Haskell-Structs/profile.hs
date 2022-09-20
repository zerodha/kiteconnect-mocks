{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Profile
    ( Profile (..)
    , Data (..)
    , Meta (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data Profile = Profile
    { profileDataProfile :: Maybe Data
    , statusProfile :: Maybe Text
    } deriving (Show)

data Data = Data
    { avatarURLData :: Maybe (Maybe Text)
    , brokerData :: Maybe Text
    , emailData :: Maybe Text
    , exchangesData :: Maybe (Vector Text)
    , metaData :: Maybe Meta
    , orderTypesData :: Maybe (Vector Text)
    , productsData :: Maybe (Vector Text)
    , userIDData :: Maybe Text
    , userNameData :: Maybe Text
    , userShortnameData :: Maybe Text
    , userTypeData :: Maybe Text
    } deriving (Show)

data Meta = Meta
    { dematConsentMeta :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Profile
decodeTopLevel = decode

instance ToJSON Profile where
    toJSON (Profile profileDataProfile statusProfile) =
        object
        [ "data" .= profileDataProfile
        , "status" .= statusProfile
        ]

instance FromJSON Profile where
    parseJSON (Object v) = Profile
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Data where
    toJSON (Data avatarURLData brokerData emailData exchangesData metaData orderTypesData productsData userIDData userNameData userShortnameData userTypeData) =
        object
        [ "avatar_url" .= avatarURLData
        , "broker" .= brokerData
        , "email" .= emailData
        , "exchanges" .= exchangesData
        , "meta" .= metaData
        , "order_types" .= orderTypesData
        , "products" .= productsData
        , "user_id" .= userIDData
        , "user_name" .= userNameData
        , "user_shortname" .= userShortnameData
        , "user_type" .= userTypeData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .:? "avatar_url"
        <*> v .:? "broker"
        <*> v .:? "email"
        <*> v .:? "exchanges"
        <*> v .:? "meta"
        <*> v .:? "order_types"
        <*> v .:? "products"
        <*> v .:? "user_id"
        <*> v .:? "user_name"
        <*> v .:? "user_shortname"
        <*> v .:? "user_type"

instance ToJSON Meta where
    toJSON (Meta dematConsentMeta) =
        object
        [ "demat_consent" .= dematConsentMeta
        ]

instance FromJSON Meta where
    parseJSON (Object v) = Meta
        <$> v .:? "demat_consent"

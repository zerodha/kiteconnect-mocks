{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module GenerateSession
    ( GenerateSession (..)
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

data GenerateSession = GenerateSession
    { generateSessionDataGenerateSession :: Maybe Data
    , statusGenerateSession :: Maybe Text
    } deriving (Show)

data Data = Data
    { accessTokenData :: Maybe Text
    , apiKeyData :: Maybe Text
    , avatarURLData :: Maybe Text
    , brokerData :: Maybe Text
    , emailData :: Maybe Text
    , enctokenData :: Maybe Text
    , exchangesData :: Maybe (Vector Text)
    , loginTimeData :: Maybe Text
    , metaData :: Maybe Meta
    , orderTypesData :: Maybe (Vector Text)
    , productsData :: Maybe (Vector Text)
    , publicTokenData :: Maybe Text
    , refreshTokenData :: Maybe Text
    , siloData :: Maybe Text
    , userIDData :: Maybe Text
    , userNameData :: Maybe Text
    , userShortnameData :: Maybe Text
    , userTypeData :: Maybe Text
    } deriving (Show)

data Meta = Meta
    { dematConsentMeta :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe GenerateSession
decodeTopLevel = decode

instance ToJSON GenerateSession where
    toJSON (GenerateSession generateSessionDataGenerateSession statusGenerateSession) =
        object
        [ "data" .= generateSessionDataGenerateSession
        , "status" .= statusGenerateSession
        ]

instance FromJSON GenerateSession where
    parseJSON (Object v) = GenerateSession
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Data where
    toJSON (Data accessTokenData apiKeyData avatarURLData brokerData emailData enctokenData exchangesData loginTimeData metaData orderTypesData productsData publicTokenData refreshTokenData siloData userIDData userNameData userShortnameData userTypeData) =
        object
        [ "access_token" .= accessTokenData
        , "api_key" .= apiKeyData
        , "avatar_url" .= avatarURLData
        , "broker" .= brokerData
        , "email" .= emailData
        , "enctoken" .= enctokenData
        , "exchanges" .= exchangesData
        , "login_time" .= loginTimeData
        , "meta" .= metaData
        , "order_types" .= orderTypesData
        , "products" .= productsData
        , "public_token" .= publicTokenData
        , "refresh_token" .= refreshTokenData
        , "silo" .= siloData
        , "user_id" .= userIDData
        , "user_name" .= userNameData
        , "user_shortname" .= userShortnameData
        , "user_type" .= userTypeData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .:? "access_token"
        <*> v .:? "api_key"
        <*> v .:? "avatar_url"
        <*> v .:? "broker"
        <*> v .:? "email"
        <*> v .:? "enctoken"
        <*> v .:? "exchanges"
        <*> v .:? "login_time"
        <*> v .:? "meta"
        <*> v .:? "order_types"
        <*> v .:? "products"
        <*> v .:? "public_token"
        <*> v .:? "refresh_token"
        <*> v .:? "silo"
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

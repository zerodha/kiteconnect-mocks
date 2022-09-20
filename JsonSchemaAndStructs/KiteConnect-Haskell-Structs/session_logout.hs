{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module SessionLogout
    ( SessionLogout (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data SessionLogout = SessionLogout
    { sessionLogoutDataSessionLogout :: Maybe Bool
    , statusSessionLogout :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe SessionLogout
decodeTopLevel = decode

instance ToJSON SessionLogout where
    toJSON (SessionLogout sessionLogoutDataSessionLogout statusSessionLogout) =
        object
        [ "data" .= sessionLogoutDataSessionLogout
        , "status" .= statusSessionLogout
        ]

instance FromJSON SessionLogout where
    parseJSON (Object v) = SessionLogout
        <$> v .:? "data"
        <*> v .:? "status"

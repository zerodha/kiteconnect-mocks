{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module ConvertPosition
    ( ConvertPosition (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data ConvertPosition = ConvertPosition
    { convertPositionDataConvertPosition :: Maybe Bool
    , statusConvertPosition :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe ConvertPosition
decodeTopLevel = decode

instance ToJSON ConvertPosition where
    toJSON (ConvertPosition convertPositionDataConvertPosition statusConvertPosition) =
        object
        [ "data" .= convertPositionDataConvertPosition
        , "status" .= statusConvertPosition
        ]

instance FromJSON ConvertPosition where
    parseJSON (Object v) = ConvertPosition
        <$> v .:? "data"
        <*> v .:? "status"

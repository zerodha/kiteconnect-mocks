{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module GttGetOrders
    ( GttGetOrders (..)
    , Datum (..)
    , Condition (..)
    , Meta (..)
    , Order (..)
    , ResultClass (..)
    , OrderResult (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data GttGetOrders = GttGetOrders
    { gttGetOrdersDataGttGetOrders :: Maybe (Vector Datum)
    , statusGttGetOrders :: Maybe Text
    } deriving (Show)

data Datum = Datum
    { conditionDatum :: Maybe Condition
    , createdAtDatum :: Maybe Text
    , expiresAtDatum :: Maybe Text
    , datumIDDatum :: Maybe Int
    , metaDatum :: Maybe Meta
    , ordersDatum :: Maybe (Vector Order)
    , parentTriggerDatum :: Maybe (Maybe Text)
    , statusDatum :: Maybe Text
    , datumTypeDatum :: Maybe Text
    , updatedAtDatum :: Maybe Text
    , userIDDatum :: Maybe Text
    } deriving (Show)

data Condition = Condition
    { exchangeCondition :: Maybe Text
    , instrumentTokenCondition :: Maybe Int
    , lastPriceCondition :: Maybe Float
    , tradingsymbolCondition :: Maybe Text
    , triggerValuesCondition :: Maybe (Vector Float)
    } deriving (Show)

data Meta = Meta
    {
    } deriving (Show)

data Order = Order
    { exchangeOrder :: Maybe Text
    , orderTypeOrder :: Maybe Text
    , priceOrder :: Maybe Float
    , productOrder :: Maybe Text
    , quantityOrder :: Maybe Int
    , resultOrder :: Maybe ResultClass
    , tradingsymbolOrder :: Maybe Text
    , transactionTypeOrder :: Maybe Text
    } deriving (Show)

data ResultClass = ResultClass
    { accountIDResultClass :: Maybe Text
    , exchangeResultClass :: Maybe Text
    , metaResultClass :: Maybe Text
    , orderResultResultClass :: Maybe OrderResult
    , orderTypeResultClass :: Maybe Text
    , priceResultClass :: Maybe Int
    , productResultClass :: Maybe Text
    , quantityResultClass :: Maybe Int
    , timestampResultClass :: Maybe Text
    , tradingsymbolResultClass :: Maybe Text
    , transactionTypeResultClass :: Maybe Text
    , triggeredAtResultClass :: Maybe Float
    , validityResultClass :: Maybe Text
    } deriving (Show)

data OrderResult = OrderResult
    { orderIDOrderResult :: Maybe Text
    , rejectionReasonOrderResult :: Maybe Text
    , statusOrderResult :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe GttGetOrders
decodeTopLevel = decode

instance ToJSON GttGetOrders where
    toJSON (GttGetOrders gttGetOrdersDataGttGetOrders statusGttGetOrders) =
        object
        [ "data" .= gttGetOrdersDataGttGetOrders
        , "status" .= statusGttGetOrders
        ]

instance FromJSON GttGetOrders where
    parseJSON (Object v) = GttGetOrders
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Datum where
    toJSON (Datum conditionDatum createdAtDatum expiresAtDatum datumIDDatum metaDatum ordersDatum parentTriggerDatum statusDatum datumTypeDatum updatedAtDatum userIDDatum) =
        object
        [ "condition" .= conditionDatum
        , "created_at" .= createdAtDatum
        , "expires_at" .= expiresAtDatum
        , "id" .= datumIDDatum
        , "meta" .= metaDatum
        , "orders" .= ordersDatum
        , "parent_trigger" .= parentTriggerDatum
        , "status" .= statusDatum
        , "type" .= datumTypeDatum
        , "updated_at" .= updatedAtDatum
        , "user_id" .= userIDDatum
        ]

instance FromJSON Datum where
    parseJSON (Object v) = Datum
        <$> v .:? "condition"
        <*> v .:? "created_at"
        <*> v .:? "expires_at"
        <*> v .:? "id"
        <*> v .:? "meta"
        <*> v .:? "orders"
        <*> v .:? "parent_trigger"
        <*> v .:? "status"
        <*> v .:? "type"
        <*> v .:? "updated_at"
        <*> v .:? "user_id"

instance ToJSON Condition where
    toJSON (Condition exchangeCondition instrumentTokenCondition lastPriceCondition tradingsymbolCondition triggerValuesCondition) =
        object
        [ "exchange" .= exchangeCondition
        , "instrument_token" .= instrumentTokenCondition
        , "last_price" .= lastPriceCondition
        , "tradingsymbol" .= tradingsymbolCondition
        , "trigger_values" .= triggerValuesCondition
        ]

instance FromJSON Condition where
    parseJSON (Object v) = Condition
        <$> v .:? "exchange"
        <*> v .:? "instrument_token"
        <*> v .:? "last_price"
        <*> v .:? "tradingsymbol"
        <*> v .:? "trigger_values"

instance ToJSON Meta where
    toJSON = \_ -> emptyObject

instance FromJSON Meta where
    parseJSON emptyObject = return Meta

instance ToJSON Order where
    toJSON (Order exchangeOrder orderTypeOrder priceOrder productOrder quantityOrder resultOrder tradingsymbolOrder transactionTypeOrder) =
        object
        [ "exchange" .= exchangeOrder
        , "order_type" .= orderTypeOrder
        , "price" .= priceOrder
        , "product" .= productOrder
        , "quantity" .= quantityOrder
        , "result" .= resultOrder
        , "tradingsymbol" .= tradingsymbolOrder
        , "transaction_type" .= transactionTypeOrder
        ]

instance FromJSON Order where
    parseJSON (Object v) = Order
        <$> v .:? "exchange"
        <*> v .:? "order_type"
        <*> v .:? "price"
        <*> v .:? "product"
        <*> v .:? "quantity"
        <*> v .:? "result"
        <*> v .:? "tradingsymbol"
        <*> v .:? "transaction_type"

instance ToJSON ResultClass where
    toJSON (ResultClass accountIDResultClass exchangeResultClass metaResultClass orderResultResultClass orderTypeResultClass priceResultClass productResultClass quantityResultClass timestampResultClass tradingsymbolResultClass transactionTypeResultClass triggeredAtResultClass validityResultClass) =
        object
        [ "account_id" .= accountIDResultClass
        , "exchange" .= exchangeResultClass
        , "meta" .= metaResultClass
        , "order_result" .= orderResultResultClass
        , "order_type" .= orderTypeResultClass
        , "price" .= priceResultClass
        , "product" .= productResultClass
        , "quantity" .= quantityResultClass
        , "timestamp" .= timestampResultClass
        , "tradingsymbol" .= tradingsymbolResultClass
        , "transaction_type" .= transactionTypeResultClass
        , "triggered_at" .= triggeredAtResultClass
        , "validity" .= validityResultClass
        ]

instance FromJSON ResultClass where
    parseJSON (Object v) = ResultClass
        <$> v .:? "account_id"
        <*> v .:? "exchange"
        <*> v .:? "meta"
        <*> v .:? "order_result"
        <*> v .:? "order_type"
        <*> v .:? "price"
        <*> v .:? "product"
        <*> v .:? "quantity"
        <*> v .:? "timestamp"
        <*> v .:? "tradingsymbol"
        <*> v .:? "transaction_type"
        <*> v .:? "triggered_at"
        <*> v .:? "validity"

instance ToJSON OrderResult where
    toJSON (OrderResult orderIDOrderResult rejectionReasonOrderResult statusOrderResult) =
        object
        [ "order_id" .= orderIDOrderResult
        , "rejection_reason" .= rejectionReasonOrderResult
        , "status" .= statusOrderResult
        ]

instance FromJSON OrderResult where
    parseJSON (Object v) = OrderResult
        <$> v .:? "order_id"
        <*> v .:? "rejection_reason"
        <*> v .:? "status"

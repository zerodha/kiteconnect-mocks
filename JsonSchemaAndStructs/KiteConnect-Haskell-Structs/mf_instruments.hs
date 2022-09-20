{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module MfInstruments
    ( MFInstruments (..)
    , MFInstrument (..)
    , Amc (..)
    , DividendType (..)
    , Plan (..)
    , SchemeType (..)
    , SettlementType (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

type MFInstruments = Vector MFInstrument

data MFInstrument = MFInstrument
    { amcMFInstrument :: Maybe Amc
    , dividendTypeMFInstrument :: Maybe DividendType
    , lastPriceMFInstrument :: Maybe Float
    , lastPriceDateMFInstrument :: Maybe Text
    , minimumAdditionalPurchaseAmountMFInstrument :: Maybe Int
    , minimumPurchaseAmountMFInstrument :: Maybe Int
    , minimumRedemptionQuantityMFInstrument :: Maybe Float
    , nameMFInstrument :: Maybe Text
    , planMFInstrument :: Maybe Plan
    , purchaseAllowedMFInstrument :: Maybe Int
    , purchaseAmountMultiplierMFInstrument :: Maybe Int
    , redemptionAllowedMFInstrument :: Maybe Int
    , redemptionQuantityMultiplierMFInstrument :: Maybe Float
    , schemeTypeMFInstrument :: Maybe SchemeType
    , settlementTypeMFInstrument :: Maybe SettlementType
    , tradingsymbolMFInstrument :: Maybe Text
    } deriving (Show)

data Amc
    = BirlaSunLifeMutualFundMFAmc
    deriving (Show)

data DividendType
    = GrowthDividendType
    | PayoutDividendType
    deriving (Show)

data Plan
    = DirectPlan
    | RegularPlan
    deriving (Show)

data SchemeType
    = BalancedSchemeType
    | DebtSchemeType
    | EquitySchemeType
    | FofSchemeType
    | LiquidSchemeType
    deriving (Show)

data SettlementType
    = T1SettlementType
    | T3SettlementType
    | T4SettlementType
    | T6SettlementType
    deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFInstruments
decodeTopLevel = decode

instance ToJSON MFInstrument where
    toJSON (MFInstrument amcMFInstrument dividendTypeMFInstrument lastPriceMFInstrument lastPriceDateMFInstrument minimumAdditionalPurchaseAmountMFInstrument minimumPurchaseAmountMFInstrument minimumRedemptionQuantityMFInstrument nameMFInstrument planMFInstrument purchaseAllowedMFInstrument purchaseAmountMultiplierMFInstrument redemptionAllowedMFInstrument redemptionQuantityMultiplierMFInstrument schemeTypeMFInstrument settlementTypeMFInstrument tradingsymbolMFInstrument) =
        object
        [ "amc" .= amcMFInstrument
        , "dividend_type" .= dividendTypeMFInstrument
        , "last_price" .= lastPriceMFInstrument
        , "last_price_date" .= lastPriceDateMFInstrument
        , "minimum_additional_purchase_amount" .= minimumAdditionalPurchaseAmountMFInstrument
        , "minimum_purchase_amount" .= minimumPurchaseAmountMFInstrument
        , "minimum_redemption_quantity" .= minimumRedemptionQuantityMFInstrument
        , "name" .= nameMFInstrument
        , "plan" .= planMFInstrument
        , "purchase_allowed" .= purchaseAllowedMFInstrument
        , "purchase_amount_multiplier" .= purchaseAmountMultiplierMFInstrument
        , "redemption_allowed" .= redemptionAllowedMFInstrument
        , "redemption_quantity_multiplier" .= redemptionQuantityMultiplierMFInstrument
        , "scheme_type" .= schemeTypeMFInstrument
        , "settlement_type" .= settlementTypeMFInstrument
        , "tradingsymbol" .= tradingsymbolMFInstrument
        ]

instance FromJSON MFInstrument where
    parseJSON (Object v) = MFInstrument
        <$> v .:? "amc"
        <*> v .:? "dividend_type"
        <*> v .:? "last_price"
        <*> v .:? "last_price_date"
        <*> v .:? "minimum_additional_purchase_amount"
        <*> v .:? "minimum_purchase_amount"
        <*> v .:? "minimum_redemption_quantity"
        <*> v .:? "name"
        <*> v .:? "plan"
        <*> v .:? "purchase_allowed"
        <*> v .:? "purchase_amount_multiplier"
        <*> v .:? "redemption_allowed"
        <*> v .:? "redemption_quantity_multiplier"
        <*> v .:? "scheme_type"
        <*> v .:? "settlement_type"
        <*> v .:? "tradingsymbol"

instance ToJSON Amc where
    toJSON BirlaSunLifeMutualFundMFAmc = "BirlaSunLifeMutualFund_MF"

instance FromJSON Amc where
    parseJSON = withText "Amc" parseText
        where
            parseText "BirlaSunLifeMutualFund_MF" = return BirlaSunLifeMutualFundMFAmc

instance ToJSON DividendType where
    toJSON GrowthDividendType = "growth"
    toJSON PayoutDividendType = "payout"

instance FromJSON DividendType where
    parseJSON = withText "DividendType" parseText
        where
            parseText "growth" = return GrowthDividendType
            parseText "payout" = return PayoutDividendType

instance ToJSON Plan where
    toJSON DirectPlan = "direct"
    toJSON RegularPlan = "regular"

instance FromJSON Plan where
    parseJSON = withText "Plan" parseText
        where
            parseText "direct" = return DirectPlan
            parseText "regular" = return RegularPlan

instance ToJSON SchemeType where
    toJSON BalancedSchemeType = "balanced"
    toJSON DebtSchemeType = "debt"
    toJSON EquitySchemeType = "equity"
    toJSON FofSchemeType = "fof"
    toJSON LiquidSchemeType = "liquid"

instance FromJSON SchemeType where
    parseJSON = withText "SchemeType" parseText
        where
            parseText "balanced" = return BalancedSchemeType
            parseText "debt" = return DebtSchemeType
            parseText "equity" = return EquitySchemeType
            parseText "fof" = return FofSchemeType
            parseText "liquid" = return LiquidSchemeType

instance ToJSON SettlementType where
    toJSON T1SettlementType = "T1"
    toJSON T3SettlementType = "T3"
    toJSON T4SettlementType = "T4"
    toJSON T6SettlementType = "T6"

instance FromJSON SettlementType where
    parseJSON = withText "SettlementType" parseText
        where
            parseText "T1" = return T1SettlementType
            parseText "T3" = return T3SettlementType
            parseText "T4" = return T4SettlementType
            parseText "T6" = return T6SettlementType

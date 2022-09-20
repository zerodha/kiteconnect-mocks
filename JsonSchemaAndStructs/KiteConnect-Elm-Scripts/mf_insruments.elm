-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import MfInstruments exposing (mfInstruments)
--
-- and you're off to the races with
--
--     decodeString mfInstruments myJsonString

module MfInstruments exposing
    ( MFInstruments
    , mfInstrumentsToString
    , mfInstruments
    , MFInstrument
    , Amc(..)
    , DividendType(..)
    , Plan(..)
    , SchemeType(..)
    , SettlementType(..)
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias MFInstruments = Array MFInstrument

type alias MFInstrument =
    { amc : Maybe Amc
    , dividendType : Maybe DividendType
    , lastPrice : Maybe Float
    , lastPriceDate : Maybe String
    , minimumAdditionalPurchaseAmount : Maybe Int
    , minimumPurchaseAmount : Maybe Int
    , minimumRedemptionQuantity : Maybe Float
    , name : Maybe String
    , plan : Maybe Plan
    , purchaseAllowed : Maybe Int
    , purchaseAmountMultiplier : Maybe Int
    , redemptionAllowed : Maybe Int
    , redemptionQuantityMultiplier : Maybe Float
    , schemeType : Maybe SchemeType
    , settlementType : Maybe SettlementType
    , tradingsymbol : Maybe String
    }

type Amc
    = BirlaSunLifeMutualFundMF

type DividendType
    = Growth
    | Payout

type Plan
    = Direct
    | Regular

type SchemeType
    = Balanced
    | Debt
    | Equity
    | Fof
    | Liquid

type SettlementType
    = T1
    | T3
    | T4
    | T6

-- decoders and encoders

mfInstruments : Jdec.Decoder MFInstruments
mfInstruments = Jdec.array mfInstrument

mfInstrumentsToString : MFInstruments -> String
mfInstrumentsToString r = Jenc.encode 0 (makeArrayEncoder encodeMFInstrument r)

mfInstrument : Jdec.Decoder MFInstrument
mfInstrument =
    Jpipe.decode MFInstrument
        |> Jpipe.optional "amc" (Jdec.nullable amc) Nothing
        |> Jpipe.optional "dividend_type" (Jdec.nullable dividendType) Nothing
        |> Jpipe.optional "last_price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "last_price_date" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "minimum_additional_purchase_amount" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "minimum_purchase_amount" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "minimum_redemption_quantity" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "name" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "plan" (Jdec.nullable plan) Nothing
        |> Jpipe.optional "purchase_allowed" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "purchase_amount_multiplier" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "redemption_allowed" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "redemption_quantity_multiplier" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "scheme_type" (Jdec.nullable schemeType) Nothing
        |> Jpipe.optional "settlement_type" (Jdec.nullable settlementType) Nothing
        |> Jpipe.optional "tradingsymbol" (Jdec.nullable Jdec.string) Nothing

encodeMFInstrument : MFInstrument -> Jenc.Value
encodeMFInstrument x =
    Jenc.object
        [ ("amc", makeNullableEncoder encodeAmc x.amc)
        , ("dividend_type", makeNullableEncoder encodeDividendType x.dividendType)
        , ("last_price", makeNullableEncoder Jenc.float x.lastPrice)
        , ("last_price_date", makeNullableEncoder Jenc.string x.lastPriceDate)
        , ("minimum_additional_purchase_amount", makeNullableEncoder Jenc.int x.minimumAdditionalPurchaseAmount)
        , ("minimum_purchase_amount", makeNullableEncoder Jenc.int x.minimumPurchaseAmount)
        , ("minimum_redemption_quantity", makeNullableEncoder Jenc.float x.minimumRedemptionQuantity)
        , ("name", makeNullableEncoder Jenc.string x.name)
        , ("plan", makeNullableEncoder encodePlan x.plan)
        , ("purchase_allowed", makeNullableEncoder Jenc.int x.purchaseAllowed)
        , ("purchase_amount_multiplier", makeNullableEncoder Jenc.int x.purchaseAmountMultiplier)
        , ("redemption_allowed", makeNullableEncoder Jenc.int x.redemptionAllowed)
        , ("redemption_quantity_multiplier", makeNullableEncoder Jenc.float x.redemptionQuantityMultiplier)
        , ("scheme_type", makeNullableEncoder encodeSchemeType x.schemeType)
        , ("settlement_type", makeNullableEncoder encodeSettlementType x.settlementType)
        , ("tradingsymbol", makeNullableEncoder Jenc.string x.tradingsymbol)
        ]

amc : Jdec.Decoder Amc
amc =
    Jdec.string
        |> Jdec.andThen (\str ->
            case str of
                "BirlaSunLifeMutualFund_MF" -> Jdec.succeed BirlaSunLifeMutualFundMF
                somethingElse -> Jdec.fail <| "Invalid Amc: " ++ somethingElse
        )

encodeAmc : Amc -> Jenc.Value
encodeAmc x = case x of
    BirlaSunLifeMutualFundMF -> Jenc.string "BirlaSunLifeMutualFund_MF"

dividendType : Jdec.Decoder DividendType
dividendType =
    Jdec.string
        |> Jdec.andThen (\str ->
            case str of
                "growth" -> Jdec.succeed Growth
                "payout" -> Jdec.succeed Payout
                somethingElse -> Jdec.fail <| "Invalid DividendType: " ++ somethingElse
        )

encodeDividendType : DividendType -> Jenc.Value
encodeDividendType x = case x of
    Growth -> Jenc.string "growth"
    Payout -> Jenc.string "payout"

plan : Jdec.Decoder Plan
plan =
    Jdec.string
        |> Jdec.andThen (\str ->
            case str of
                "direct" -> Jdec.succeed Direct
                "regular" -> Jdec.succeed Regular
                somethingElse -> Jdec.fail <| "Invalid Plan: " ++ somethingElse
        )

encodePlan : Plan -> Jenc.Value
encodePlan x = case x of
    Direct -> Jenc.string "direct"
    Regular -> Jenc.string "regular"

schemeType : Jdec.Decoder SchemeType
schemeType =
    Jdec.string
        |> Jdec.andThen (\str ->
            case str of
                "balanced" -> Jdec.succeed Balanced
                "debt" -> Jdec.succeed Debt
                "equity" -> Jdec.succeed Equity
                "fof" -> Jdec.succeed Fof
                "liquid" -> Jdec.succeed Liquid
                somethingElse -> Jdec.fail <| "Invalid SchemeType: " ++ somethingElse
        )

encodeSchemeType : SchemeType -> Jenc.Value
encodeSchemeType x = case x of
    Balanced -> Jenc.string "balanced"
    Debt -> Jenc.string "debt"
    Equity -> Jenc.string "equity"
    Fof -> Jenc.string "fof"
    Liquid -> Jenc.string "liquid"

settlementType : Jdec.Decoder SettlementType
settlementType =
    Jdec.string
        |> Jdec.andThen (\str ->
            case str of
                "T1" -> Jdec.succeed T1
                "T3" -> Jdec.succeed T3
                "T4" -> Jdec.succeed T4
                "T6" -> Jdec.succeed T6
                somethingElse -> Jdec.fail <| "Invalid SettlementType: " ++ somethingElse
        )

encodeSettlementType : SettlementType -> Jenc.Value
encodeSettlementType x = case x of
    T1 -> Jenc.string "T1"
    T3 -> Jenc.string "T3"
    T4 -> Jenc.string "T4"
    T6 -> Jenc.string "T6"

--- encoder helpers

makeArrayEncoder : (a -> Jenc.Value) -> Array a -> Jenc.Value
makeArrayEncoder f arr =
    Jenc.array (Array.map f arr)

makeDictEncoder : (a -> Jenc.Value) -> Dict String a -> Jenc.Value
makeDictEncoder f dict =
    Jenc.object (toList (Dict.map (\k -> f) dict))

makeNullableEncoder : (a -> Jenc.Value) -> Maybe a -> Jenc.Value
makeNullableEncoder f m =
    case m of
    Just x -> f x
    Nothing -> Jenc.null

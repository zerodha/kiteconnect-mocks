-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import TickerQuote exposing (tickerQuote)
--
-- and you're off to the races with
--
--     decodeString tickerQuote myJsonString

module TickerQuote exposing
    ( TickerQuote
    , tickerQuoteToString
    , tickerQuote
    , TriggerRangeElement
    , Ohlc
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias TickerQuote = Array TriggerRangeElement

type alias TriggerRangeElement =
    { averageTradedPrice : Maybe Float
    , change : Maybe Float
    , instrumentToken : Maybe Int
    , lastPrice : Maybe Int
    , lastTradedQuantity : Maybe Int
    , mode : Maybe String
    , ohlc : Maybe Ohlc
    , totalBuyQuantity : Maybe Int
    , totalSellQuantity : Maybe Int
    , tradable : Maybe Bool
    , volumeTraded : Maybe Int
    }

type alias Ohlc =
    { close : Maybe Int
    , high : Maybe Int
    , low : Maybe Int
    , open : Maybe Int
    }

-- decoders and encoders

tickerQuote : Jdec.Decoder TickerQuote
tickerQuote = Jdec.array triggerRangeElement

tickerQuoteToString : TickerQuote -> String
tickerQuoteToString r = Jenc.encode 0 (makeArrayEncoder encodeTriggerRangeElement r)

triggerRangeElement : Jdec.Decoder TriggerRangeElement
triggerRangeElement =
    Jpipe.decode TriggerRangeElement
        |> Jpipe.optional "average_traded_price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "change" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "instrument_token" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "last_price" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "last_traded_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "mode" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "ohlc" (Jdec.nullable ohlc) Nothing
        |> Jpipe.optional "total_buy_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "total_sell_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "tradable" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "volume_traded" (Jdec.nullable Jdec.int) Nothing

encodeTriggerRangeElement : TriggerRangeElement -> Jenc.Value
encodeTriggerRangeElement x =
    Jenc.object
        [ ("average_traded_price", makeNullableEncoder Jenc.float x.averageTradedPrice)
        , ("change", makeNullableEncoder Jenc.float x.change)
        , ("instrument_token", makeNullableEncoder Jenc.int x.instrumentToken)
        , ("last_price", makeNullableEncoder Jenc.int x.lastPrice)
        , ("last_traded_quantity", makeNullableEncoder Jenc.int x.lastTradedQuantity)
        , ("mode", makeNullableEncoder Jenc.string x.mode)
        , ("ohlc", makeNullableEncoder encodeOhlc x.ohlc)
        , ("total_buy_quantity", makeNullableEncoder Jenc.int x.totalBuyQuantity)
        , ("total_sell_quantity", makeNullableEncoder Jenc.int x.totalSellQuantity)
        , ("tradable", makeNullableEncoder Jenc.bool x.tradable)
        , ("volume_traded", makeNullableEncoder Jenc.int x.volumeTraded)
        ]

ohlc : Jdec.Decoder Ohlc
ohlc =
    Jpipe.decode Ohlc
        |> Jpipe.optional "close" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "high" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "low" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "open" (Jdec.nullable Jdec.int) Nothing

encodeOhlc : Ohlc -> Jenc.Value
encodeOhlc x =
    Jenc.object
        [ ("close", makeNullableEncoder Jenc.int x.close)
        , ("high", makeNullableEncoder Jenc.int x.high)
        , ("low", makeNullableEncoder Jenc.int x.low)
        , ("open", makeNullableEncoder Jenc.int x.open)
        ]

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

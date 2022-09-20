-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import TickerFull exposing (tickerFull)
--
-- and you're off to the races with
--
--     decodeString tickerFull myJsonString

module TickerFull exposing
    ( TickerFull
    , tickerFullToString
    , tickerFull
    , TriggerRangeElement
    , Depth
    , Buy
    , Ohlc
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias TickerFull = Array TriggerRangeElement

type alias TriggerRangeElement =
    { averageTradedPrice : Maybe Float
    , change : Maybe Float
    , depth : Maybe Depth
    , exchangeTimestamp : Maybe String
    , instrumentToken : Maybe Int
    , lastPrice : Maybe Int
    , lastTradeTime : Maybe String
    , lastTradedQuantity : Maybe Int
    , mode : Maybe String
    , ohlc : Maybe Ohlc
    , oi : Maybe Int
    , oiDayHigh : Maybe Int
    , oiDayLow : Maybe Int
    , totalBuyQuantity : Maybe Int
    , totalSellQuantity : Maybe Int
    , tradable : Maybe Bool
    , volumeTraded : Maybe Int
    }

type alias Depth =
    { buy : Maybe (Array Buy)
    , sell : Maybe (Array Buy)
    }

type alias Buy =
    { orders : Maybe Int
    , price : Maybe Int
    , quantity : Maybe Int
    }

type alias Ohlc =
    { close : Maybe Int
    , high : Maybe Int
    , low : Maybe Int
    , open : Maybe Int
    }

-- decoders and encoders

tickerFull : Jdec.Decoder TickerFull
tickerFull = Jdec.array triggerRangeElement

tickerFullToString : TickerFull -> String
tickerFullToString r = Jenc.encode 0 (makeArrayEncoder encodeTriggerRangeElement r)

triggerRangeElement : Jdec.Decoder TriggerRangeElement
triggerRangeElement =
    Jpipe.decode TriggerRangeElement
        |> Jpipe.optional "average_traded_price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "change" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "depth" (Jdec.nullable depth) Nothing
        |> Jpipe.optional "exchange_timestamp" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "instrument_token" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "last_price" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "last_trade_time" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "last_traded_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "mode" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "ohlc" (Jdec.nullable ohlc) Nothing
        |> Jpipe.optional "oi" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "oi_day_high" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "oi_day_low" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "total_buy_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "total_sell_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "tradable" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "volume_traded" (Jdec.nullable Jdec.int) Nothing

encodeTriggerRangeElement : TriggerRangeElement -> Jenc.Value
encodeTriggerRangeElement x =
    Jenc.object
        [ ("average_traded_price", makeNullableEncoder Jenc.float x.averageTradedPrice)
        , ("change", makeNullableEncoder Jenc.float x.change)
        , ("depth", makeNullableEncoder encodeDepth x.depth)
        , ("exchange_timestamp", makeNullableEncoder Jenc.string x.exchangeTimestamp)
        , ("instrument_token", makeNullableEncoder Jenc.int x.instrumentToken)
        , ("last_price", makeNullableEncoder Jenc.int x.lastPrice)
        , ("last_trade_time", makeNullableEncoder Jenc.string x.lastTradeTime)
        , ("last_traded_quantity", makeNullableEncoder Jenc.int x.lastTradedQuantity)
        , ("mode", makeNullableEncoder Jenc.string x.mode)
        , ("ohlc", makeNullableEncoder encodeOhlc x.ohlc)
        , ("oi", makeNullableEncoder Jenc.int x.oi)
        , ("oi_day_high", makeNullableEncoder Jenc.int x.oiDayHigh)
        , ("oi_day_low", makeNullableEncoder Jenc.int x.oiDayLow)
        , ("total_buy_quantity", makeNullableEncoder Jenc.int x.totalBuyQuantity)
        , ("total_sell_quantity", makeNullableEncoder Jenc.int x.totalSellQuantity)
        , ("tradable", makeNullableEncoder Jenc.bool x.tradable)
        , ("volume_traded", makeNullableEncoder Jenc.int x.volumeTraded)
        ]

depth : Jdec.Decoder Depth
depth =
    Jpipe.decode Depth
        |> Jpipe.optional "buy" (Jdec.nullable (Jdec.array buy)) Nothing
        |> Jpipe.optional "sell" (Jdec.nullable (Jdec.array buy)) Nothing

encodeDepth : Depth -> Jenc.Value
encodeDepth x =
    Jenc.object
        [ ("buy", makeNullableEncoder (makeArrayEncoder encodeBuy) x.buy)
        , ("sell", makeNullableEncoder (makeArrayEncoder encodeBuy) x.sell)
        ]

buy : Jdec.Decoder Buy
buy =
    Jpipe.decode Buy
        |> Jpipe.optional "orders" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "price" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "quantity" (Jdec.nullable Jdec.int) Nothing

encodeBuy : Buy -> Jenc.Value
encodeBuy x =
    Jenc.object
        [ ("orders", makeNullableEncoder Jenc.int x.orders)
        , ("price", makeNullableEncoder Jenc.int x.price)
        , ("quantity", makeNullableEncoder Jenc.int x.quantity)
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

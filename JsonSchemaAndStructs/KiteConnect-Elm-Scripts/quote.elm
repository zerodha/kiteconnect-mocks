-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import Quote exposing (quote)
--
-- and you're off to the races with
--
--     decodeString quote myJsonString

module Quote exposing
    ( Quote
    , quoteToString
    , quote
    , Datum
    , Depth
    , Buy
    , Ohlc
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias Quote =
    { data : Maybe (Dict String Datum)
    , status : Maybe String
    }

type alias Datum =
    { averagePrice : Maybe Float
    , buyQuantity : Maybe Int
    , depth : Maybe Depth
    , instrumentToken : Maybe Int
    , lastPrice : Maybe Float
    , lastQuantity : Maybe Int
    , lastTradeTime : Maybe String
    , lowerCircuitLimit : Maybe Float
    , netChange : Maybe Int
    , ohlc : Maybe Ohlc
    , oi : Maybe Int
    , oiDayHigh : Maybe Int
    , oiDayLow : Maybe Int
    , sellQuantity : Maybe Int
    , timestamp : Maybe String
    , upperCircuitLimit : Maybe Float
    , volume : Maybe Int
    }

type alias Depth =
    { buy : Maybe (Array Buy)
    , sell : Maybe (Array Buy)
    }

type alias Buy =
    { orders : Maybe Int
    , price : Maybe Float
    , quantity : Maybe Int
    }

type alias Ohlc =
    { close : Maybe Float
    , high : Maybe Float
    , low : Maybe Float
    , open : Maybe Int
    }

-- decoders and encoders

quoteToString : Quote -> String
quoteToString r = Jenc.encode 0 (encodeQuote r)

quote : Jdec.Decoder Quote
quote =
    Jpipe.decode Quote
        |> Jpipe.optional "data" (Jdec.nullable (Jdec.dict datum)) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeQuote : Quote -> Jenc.Value
encodeQuote x =
    Jenc.object
        [ ("data", makeNullableEncoder (makeDictEncoder encodeDatum) x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
        ]

datum : Jdec.Decoder Datum
datum =
    Jpipe.decode Datum
        |> Jpipe.optional "average_price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "buy_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "depth" (Jdec.nullable depth) Nothing
        |> Jpipe.optional "instrument_token" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "last_price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "last_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "last_trade_time" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "lower_circuit_limit" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "net_change" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "ohlc" (Jdec.nullable ohlc) Nothing
        |> Jpipe.optional "oi" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "oi_day_high" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "oi_day_low" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "sell_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "timestamp" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "upper_circuit_limit" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "volume" (Jdec.nullable Jdec.int) Nothing

encodeDatum : Datum -> Jenc.Value
encodeDatum x =
    Jenc.object
        [ ("average_price", makeNullableEncoder Jenc.float x.averagePrice)
        , ("buy_quantity", makeNullableEncoder Jenc.int x.buyQuantity)
        , ("depth", makeNullableEncoder encodeDepth x.depth)
        , ("instrument_token", makeNullableEncoder Jenc.int x.instrumentToken)
        , ("last_price", makeNullableEncoder Jenc.float x.lastPrice)
        , ("last_quantity", makeNullableEncoder Jenc.int x.lastQuantity)
        , ("last_trade_time", makeNullableEncoder Jenc.string x.lastTradeTime)
        , ("lower_circuit_limit", makeNullableEncoder Jenc.float x.lowerCircuitLimit)
        , ("net_change", makeNullableEncoder Jenc.int x.netChange)
        , ("ohlc", makeNullableEncoder encodeOhlc x.ohlc)
        , ("oi", makeNullableEncoder Jenc.int x.oi)
        , ("oi_day_high", makeNullableEncoder Jenc.int x.oiDayHigh)
        , ("oi_day_low", makeNullableEncoder Jenc.int x.oiDayLow)
        , ("sell_quantity", makeNullableEncoder Jenc.int x.sellQuantity)
        , ("timestamp", makeNullableEncoder Jenc.string x.timestamp)
        , ("upper_circuit_limit", makeNullableEncoder Jenc.float x.upperCircuitLimit)
        , ("volume", makeNullableEncoder Jenc.int x.volume)
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
        |> Jpipe.optional "price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "quantity" (Jdec.nullable Jdec.int) Nothing

encodeBuy : Buy -> Jenc.Value
encodeBuy x =
    Jenc.object
        [ ("orders", makeNullableEncoder Jenc.int x.orders)
        , ("price", makeNullableEncoder Jenc.float x.price)
        , ("quantity", makeNullableEncoder Jenc.int x.quantity)
        ]

ohlc : Jdec.Decoder Ohlc
ohlc =
    Jpipe.decode Ohlc
        |> Jpipe.optional "close" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "high" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "low" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "open" (Jdec.nullable Jdec.int) Nothing

encodeOhlc : Ohlc -> Jenc.Value
encodeOhlc x =
    Jenc.object
        [ ("close", makeNullableEncoder Jenc.float x.close)
        , ("high", makeNullableEncoder Jenc.float x.high)
        , ("low", makeNullableEncoder Jenc.float x.low)
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

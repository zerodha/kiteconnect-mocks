-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import Positions exposing (positions)
--
-- and you're off to the races with
--
--     decodeString positions myJsonString

module Positions exposing
    ( Positions
    , positionsToString
    , positions
    , Data
    , Day
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias Positions =
    { data : Maybe Data
    , status : Maybe String
    }

type alias Data =
    { day : Maybe (Array Day)
    , net : Maybe (Array Day)
    }

type alias Day =
    { averagePrice : Maybe Float
    , buyM2M : Maybe Int
    , buyPrice : Maybe Float
    , buyQuantity : Maybe Int
    , buyValue : Maybe Int
    , closePrice : Maybe Int
    , dayBuyPrice : Maybe Float
    , dayBuyQuantity : Maybe Int
    , dayBuyValue : Maybe Int
    , daySellPrice : Maybe Int
    , daySellQuantity : Maybe Int
    , daySellValue : Maybe Int
    , exchange : Maybe String
    , instrumentToken : Maybe Int
    , lastPrice : Maybe Float
    , m2M : Maybe Int
    , multiplier : Maybe Int
    , overnightQuantity : Maybe Int
    , pnl : Maybe Int
    , product : Maybe String
    , quantity : Maybe Int
    , realised : Maybe Int
    , sellM2M : Maybe Int
    , sellPrice : Maybe Int
    , sellQuantity : Maybe Int
    , sellValue : Maybe Int
    , tradingsymbol : Maybe String
    , unrealised : Maybe Int
    , value : Maybe Int
    }

-- decoders and encoders

positionsToString : Positions -> String
positionsToString r = Jenc.encode 0 (encodePositions r)

positions : Jdec.Decoder Positions
positions =
    Jpipe.decode Positions
        |> Jpipe.optional "data" (Jdec.nullable data) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodePositions : Positions -> Jenc.Value
encodePositions x =
    Jenc.object
        [ ("data", makeNullableEncoder encodeData x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
        ]

data : Jdec.Decoder Data
data =
    Jpipe.decode Data
        |> Jpipe.optional "day" (Jdec.nullable (Jdec.array day)) Nothing
        |> Jpipe.optional "net" (Jdec.nullable (Jdec.array day)) Nothing

encodeData : Data -> Jenc.Value
encodeData x =
    Jenc.object
        [ ("day", makeNullableEncoder (makeArrayEncoder encodeDay) x.day)
        , ("net", makeNullableEncoder (makeArrayEncoder encodeDay) x.net)
        ]

day : Jdec.Decoder Day
day =
    Jpipe.decode Day
        |> Jpipe.optional "average_price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "buy_m2m" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "buy_price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "buy_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "buy_value" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "close_price" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "day_buy_price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "day_buy_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "day_buy_value" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "day_sell_price" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "day_sell_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "day_sell_value" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "exchange" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "instrument_token" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "last_price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "m2m" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "multiplier" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "overnight_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "pnl" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "product" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "realised" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "sell_m2m" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "sell_price" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "sell_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "sell_value" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "tradingsymbol" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "unrealised" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "value" (Jdec.nullable Jdec.int) Nothing

encodeDay : Day -> Jenc.Value
encodeDay x =
    Jenc.object
        [ ("average_price", makeNullableEncoder Jenc.float x.averagePrice)
        , ("buy_m2m", makeNullableEncoder Jenc.int x.buyM2M)
        , ("buy_price", makeNullableEncoder Jenc.float x.buyPrice)
        , ("buy_quantity", makeNullableEncoder Jenc.int x.buyQuantity)
        , ("buy_value", makeNullableEncoder Jenc.int x.buyValue)
        , ("close_price", makeNullableEncoder Jenc.int x.closePrice)
        , ("day_buy_price", makeNullableEncoder Jenc.float x.dayBuyPrice)
        , ("day_buy_quantity", makeNullableEncoder Jenc.int x.dayBuyQuantity)
        , ("day_buy_value", makeNullableEncoder Jenc.int x.dayBuyValue)
        , ("day_sell_price", makeNullableEncoder Jenc.int x.daySellPrice)
        , ("day_sell_quantity", makeNullableEncoder Jenc.int x.daySellQuantity)
        , ("day_sell_value", makeNullableEncoder Jenc.int x.daySellValue)
        , ("exchange", makeNullableEncoder Jenc.string x.exchange)
        , ("instrument_token", makeNullableEncoder Jenc.int x.instrumentToken)
        , ("last_price", makeNullableEncoder Jenc.float x.lastPrice)
        , ("m2m", makeNullableEncoder Jenc.int x.m2M)
        , ("multiplier", makeNullableEncoder Jenc.int x.multiplier)
        , ("overnight_quantity", makeNullableEncoder Jenc.int x.overnightQuantity)
        , ("pnl", makeNullableEncoder Jenc.int x.pnl)
        , ("product", makeNullableEncoder Jenc.string x.product)
        , ("quantity", makeNullableEncoder Jenc.int x.quantity)
        , ("realised", makeNullableEncoder Jenc.int x.realised)
        , ("sell_m2m", makeNullableEncoder Jenc.int x.sellM2M)
        , ("sell_price", makeNullableEncoder Jenc.int x.sellPrice)
        , ("sell_quantity", makeNullableEncoder Jenc.int x.sellQuantity)
        , ("sell_value", makeNullableEncoder Jenc.int x.sellValue)
        , ("tradingsymbol", makeNullableEncoder Jenc.string x.tradingsymbol)
        , ("unrealised", makeNullableEncoder Jenc.int x.unrealised)
        , ("value", makeNullableEncoder Jenc.int x.value)
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

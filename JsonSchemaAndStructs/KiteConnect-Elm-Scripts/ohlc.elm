-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import Ohlc exposing (ohlc)
--
-- and you're off to the races with
--
--     decodeString ohlc myJsonString

module Ohlc exposing
    ( Ohlc
    , ohlcToString
    , ohlc
    , Datum
    , OhlcClass
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias Ohlc =
    { data : Maybe (Dict String Datum)
    , status : Maybe String
    }

type alias Datum =
    { instrumentToken : Maybe Int
    , lastPrice : Maybe Int
    , ohlc : Maybe OhlcClass
    }

type alias OhlcClass =
    { close : Maybe Float
    , high : Maybe Float
    , low : Maybe Float
    , open : Maybe Float
    }

-- decoders and encoders

ohlcToString : Ohlc -> String
ohlcToString r = Jenc.encode 0 (encodeOhlc r)

ohlc : Jdec.Decoder Ohlc
ohlc =
    Jpipe.decode Ohlc
        |> Jpipe.optional "data" (Jdec.nullable (Jdec.dict datum)) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeOhlc : Ohlc -> Jenc.Value
encodeOhlc x =
    Jenc.object
        [ ("data", makeNullableEncoder (makeDictEncoder encodeDatum) x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
        ]

datum : Jdec.Decoder Datum
datum =
    Jpipe.decode Datum
        |> Jpipe.optional "instrument_token" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "last_price" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "ohlc" (Jdec.nullable ohlcClass) Nothing

encodeDatum : Datum -> Jenc.Value
encodeDatum x =
    Jenc.object
        [ ("instrument_token", makeNullableEncoder Jenc.int x.instrumentToken)
        , ("last_price", makeNullableEncoder Jenc.int x.lastPrice)
        , ("ohlc", makeNullableEncoder encodeOhlcClass x.ohlc)
        ]

ohlcClass : Jdec.Decoder OhlcClass
ohlcClass =
    Jpipe.decode OhlcClass
        |> Jpipe.optional "close" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "high" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "low" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "open" (Jdec.nullable Jdec.float) Nothing

encodeOhlcClass : OhlcClass -> Jenc.Value
encodeOhlcClass x =
    Jenc.object
        [ ("close", makeNullableEncoder Jenc.float x.close)
        , ("high", makeNullableEncoder Jenc.float x.high)
        , ("low", makeNullableEncoder Jenc.float x.low)
        , ("open", makeNullableEncoder Jenc.float x.open)
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

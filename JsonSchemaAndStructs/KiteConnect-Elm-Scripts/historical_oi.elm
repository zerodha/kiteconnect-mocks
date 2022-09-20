-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import HistoricalOi exposing (historicalOi)
--
-- and you're off to the races with
--
--     decodeString historicalOi myJsonString

module HistoricalOi exposing
    ( HistoricalOi
    , historicalOiToString
    , historicalOi
    , Data
    , Candle(..)
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias HistoricalOi =
    { data : Maybe Data
    , status : Maybe String
    }

type alias Data =
    { candles : Maybe (Array (Array Candle))
    }

type Candle
    = DoubleInCandle Float
    | StringInCandle String

-- decoders and encoders

historicalOiToString : HistoricalOi -> String
historicalOiToString r = Jenc.encode 0 (encodeHistoricalOi r)

historicalOi : Jdec.Decoder HistoricalOi
historicalOi =
    Jpipe.decode HistoricalOi
        |> Jpipe.optional "data" (Jdec.nullable data) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeHistoricalOi : HistoricalOi -> Jenc.Value
encodeHistoricalOi x =
    Jenc.object
        [ ("data", makeNullableEncoder encodeData x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
        ]

data : Jdec.Decoder Data
data =
    Jpipe.decode Data
        |> Jpipe.optional "candles" (Jdec.nullable (Jdec.array (Jdec.array candle))) Nothing

encodeData : Data -> Jenc.Value
encodeData x =
    Jenc.object
        [ ("candles", makeNullableEncoder (makeArrayEncoder (makeArrayEncoder encodeCandle)) x.candles)
        ]

candle : Jdec.Decoder Candle
candle =
    Jdec.oneOf
        [ Jdec.map StringInCandle Jdec.string
        , Jdec.map DoubleInCandle Jdec.float
        ]

encodeCandle : Candle -> Jenc.Value
encodeCandle x = case x of
    StringInCandle y -> Jenc.string y
    DoubleInCandle y -> Jenc.float y

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

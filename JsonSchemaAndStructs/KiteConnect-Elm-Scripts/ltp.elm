-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import Ltp exposing (ltp)
--
-- and you're off to the races with
--
--     decodeString ltp myJsonString

module Ltp exposing
    ( Ltp
    , ltpToString
    , ltp
    , Datum
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias Ltp =
    { data : Maybe (Dict String Datum)
    , status : Maybe String
    }

type alias Datum =
    { instrumentToken : Maybe Int
    , lastPrice : Maybe Float
    }

-- decoders and encoders

ltpToString : Ltp -> String
ltpToString r = Jenc.encode 0 (encodeLtp r)

ltp : Jdec.Decoder Ltp
ltp =
    Jpipe.decode Ltp
        |> Jpipe.optional "data" (Jdec.nullable (Jdec.dict datum)) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeLtp : Ltp -> Jenc.Value
encodeLtp x =
    Jenc.object
        [ ("data", makeNullableEncoder (makeDictEncoder encodeDatum) x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
        ]

datum : Jdec.Decoder Datum
datum =
    Jpipe.decode Datum
        |> Jpipe.optional "instrument_token" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "last_price" (Jdec.nullable Jdec.float) Nothing

encodeDatum : Datum -> Jenc.Value
encodeDatum x =
    Jenc.object
        [ ("instrument_token", makeNullableEncoder Jenc.int x.instrumentToken)
        , ("last_price", makeNullableEncoder Jenc.float x.lastPrice)
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

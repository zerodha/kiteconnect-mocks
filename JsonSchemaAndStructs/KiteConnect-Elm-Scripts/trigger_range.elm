-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import TriggerRange exposing (triggerRange)
--
-- and you're off to the races with
--
--     decodeString triggerRange myJsonString

module TriggerRange exposing
    ( TriggerRange
    , triggerRangeToString
    , triggerRange
    , Datum
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias TriggerRange =
    { data : Maybe (Dict String Datum)
    , status : Maybe String
    }

type alias Datum =
    { instrumentToken : Maybe Int
    , lower : Maybe Float
    , upper : Maybe Float
    }

-- decoders and encoders

triggerRangeToString : TriggerRange -> String
triggerRangeToString r = Jenc.encode 0 (encodeTriggerRange r)

triggerRange : Jdec.Decoder TriggerRange
triggerRange =
    Jpipe.decode TriggerRange
        |> Jpipe.optional "data" (Jdec.nullable (Jdec.dict datum)) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeTriggerRange : TriggerRange -> Jenc.Value
encodeTriggerRange x =
    Jenc.object
        [ ("data", makeNullableEncoder (makeDictEncoder encodeDatum) x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
        ]

datum : Jdec.Decoder Datum
datum =
    Jpipe.decode Datum
        |> Jpipe.optional "instrument_token" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "lower" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "upper" (Jdec.nullable Jdec.float) Nothing

encodeDatum : Datum -> Jenc.Value
encodeDatum x =
    Jenc.object
        [ ("instrument_token", makeNullableEncoder Jenc.int x.instrumentToken)
        , ("lower", makeNullableEncoder Jenc.float x.lower)
        , ("upper", makeNullableEncoder Jenc.float x.upper)
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

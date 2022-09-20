-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import GttModifyOrder exposing (gttModifyOrder)
--
-- and you're off to the races with
--
--     decodeString gttModifyOrder myJsonString

module GttModifyOrder exposing
    ( GttModifyOrder
    , gttModifyOrderToString
    , gttModifyOrder
    , Data
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias GttModifyOrder =
    { data : Maybe Data
    , status : Maybe String
    }

type alias Data =
    { triggerID : Maybe Int
    }

-- decoders and encoders

gttModifyOrderToString : GttModifyOrder -> String
gttModifyOrderToString r = Jenc.encode 0 (encodeGttModifyOrder r)

gttModifyOrder : Jdec.Decoder GttModifyOrder
gttModifyOrder =
    Jpipe.decode GttModifyOrder
        |> Jpipe.optional "data" (Jdec.nullable data) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeGttModifyOrder : GttModifyOrder -> Jenc.Value
encodeGttModifyOrder x =
    Jenc.object
        [ ("data", makeNullableEncoder encodeData x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
        ]

data : Jdec.Decoder Data
data =
    Jpipe.decode Data
        |> Jpipe.optional "trigger_id" (Jdec.nullable Jdec.int) Nothing

encodeData : Data -> Jenc.Value
encodeData x =
    Jenc.object
        [ ("trigger_id", makeNullableEncoder Jenc.int x.triggerID)
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

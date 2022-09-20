-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import MfSipCancel exposing (mfsipCancel)
--
-- and you're off to the races with
--
--     decodeString mfsipCancel myJsonString

module MfSipCancel exposing
    ( MFSIPCancel
    , mfsipCancelToString
    , mfsipCancel
    , Data
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias MFSIPCancel =
    { data : Maybe Data
    , status : Maybe String
    }

type alias Data =
    { sipID : Maybe String
    }

-- decoders and encoders

mfsipCancelToString : MFSIPCancel -> String
mfsipCancelToString r = Jenc.encode 0 (encodeMFSIPCancel r)

mfsipCancel : Jdec.Decoder MFSIPCancel
mfsipCancel =
    Jpipe.decode MFSIPCancel
        |> Jpipe.optional "data" (Jdec.nullable data) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeMFSIPCancel : MFSIPCancel -> Jenc.Value
encodeMFSIPCancel x =
    Jenc.object
        [ ("data", makeNullableEncoder encodeData x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
        ]

data : Jdec.Decoder Data
data =
    Jpipe.decode Data
        |> Jpipe.optional "sip_id" (Jdec.nullable Jdec.string) Nothing

encodeData : Data -> Jenc.Value
encodeData x =
    Jenc.object
        [ ("sip_id", makeNullableEncoder Jenc.string x.sipID)
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

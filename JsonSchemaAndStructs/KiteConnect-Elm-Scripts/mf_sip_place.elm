-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import MfSipPlace exposing (mfsipPlace)
--
-- and you're off to the races with
--
--     decodeString mfsipPlace myJsonString

module MfSipPlace exposing
    ( MFSIPPlace
    , mfsipPlaceToString
    , mfsipPlace
    , Data
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias MFSIPPlace =
    { data : Maybe Data
    , status : Maybe String
    }

type alias Data =
    { sipID : Maybe String
    }

-- decoders and encoders

mfsipPlaceToString : MFSIPPlace -> String
mfsipPlaceToString r = Jenc.encode 0 (encodeMFSIPPlace r)

mfsipPlace : Jdec.Decoder MFSIPPlace
mfsipPlace =
    Jpipe.decode MFSIPPlace
        |> Jpipe.optional "data" (Jdec.nullable data) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeMFSIPPlace : MFSIPPlace -> Jenc.Value
encodeMFSIPPlace x =
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

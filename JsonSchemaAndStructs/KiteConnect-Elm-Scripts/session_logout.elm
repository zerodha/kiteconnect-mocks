-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import SessionLogout exposing (sessionLogout)
--
-- and you're off to the races with
--
--     decodeString sessionLogout myJsonString

module SessionLogout exposing
    ( SessionLogout
    , sessionLogoutToString
    , sessionLogout
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias SessionLogout =
    { data : Maybe Bool
    , status : Maybe String
    }

-- decoders and encoders

sessionLogoutToString : SessionLogout -> String
sessionLogoutToString r = Jenc.encode 0 (encodeSessionLogout r)

sessionLogout : Jdec.Decoder SessionLogout
sessionLogout =
    Jpipe.decode SessionLogout
        |> Jpipe.optional "data" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeSessionLogout : SessionLogout -> Jenc.Value
encodeSessionLogout x =
    Jenc.object
        [ ("data", makeNullableEncoder Jenc.bool x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
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

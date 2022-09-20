-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import MfOrderResponse exposing (mfOrderResponse)
--
-- and you're off to the races with
--
--     decodeString mfOrderResponse myJsonString

module MfOrderResponse exposing
    ( MFOrderResponse
    , mfOrderResponseToString
    , mfOrderResponse
    , Data
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias MFOrderResponse =
    { data : Maybe Data
    , status : Maybe String
    }

type alias Data =
    { orderID : Maybe String
    }

-- decoders and encoders

mfOrderResponseToString : MFOrderResponse -> String
mfOrderResponseToString r = Jenc.encode 0 (encodeMFOrderResponse r)

mfOrderResponse : Jdec.Decoder MFOrderResponse
mfOrderResponse =
    Jpipe.decode MFOrderResponse
        |> Jpipe.optional "data" (Jdec.nullable data) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeMFOrderResponse : MFOrderResponse -> Jenc.Value
encodeMFOrderResponse x =
    Jenc.object
        [ ("data", makeNullableEncoder encodeData x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
        ]

data : Jdec.Decoder Data
data =
    Jpipe.decode Data
        |> Jpipe.optional "order_id" (Jdec.nullable Jdec.string) Nothing

encodeData : Data -> Jenc.Value
encodeData x =
    Jenc.object
        [ ("order_id", makeNullableEncoder Jenc.string x.orderID)
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

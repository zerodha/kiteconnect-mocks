-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import Margins exposing (margins)
--
-- and you're off to the races with
--
--     decodeString margins myJsonString

module Margins exposing
    ( Margins
    , marginsToString
    , margins
    , Data
    , Ity
    , Available
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias Margins =
    { data : Maybe Data
    , status : Maybe String
    }

type alias Data =
    { commodity : Maybe Ity
    , equity : Maybe Ity
    }

type alias Ity =
    { available : Maybe Available
    , enabled : Maybe Bool
    , net : Maybe Float
    , utilised : Maybe (Dict String Float)
    }

type alias Available =
    { adhocMargin : Maybe Int
    , cash : Maybe Float
    , collateral : Maybe Int
    , intradayPayin : Maybe Int
    , liveBalance : Maybe Float
    , openingBalance : Maybe Float
    }

-- decoders and encoders

marginsToString : Margins -> String
marginsToString r = Jenc.encode 0 (encodeMargins r)

margins : Jdec.Decoder Margins
margins =
    Jpipe.decode Margins
        |> Jpipe.optional "data" (Jdec.nullable data) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeMargins : Margins -> Jenc.Value
encodeMargins x =
    Jenc.object
        [ ("data", makeNullableEncoder encodeData x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
        ]

data : Jdec.Decoder Data
data =
    Jpipe.decode Data
        |> Jpipe.optional "commodity" (Jdec.nullable ity) Nothing
        |> Jpipe.optional "equity" (Jdec.nullable ity) Nothing

encodeData : Data -> Jenc.Value
encodeData x =
    Jenc.object
        [ ("commodity", makeNullableEncoder encodeIty x.commodity)
        , ("equity", makeNullableEncoder encodeIty x.equity)
        ]

ity : Jdec.Decoder Ity
ity =
    Jpipe.decode Ity
        |> Jpipe.optional "available" (Jdec.nullable available) Nothing
        |> Jpipe.optional "enabled" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "net" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "utilised" (Jdec.nullable (Jdec.dict Jdec.float)) Nothing

encodeIty : Ity -> Jenc.Value
encodeIty x =
    Jenc.object
        [ ("available", makeNullableEncoder encodeAvailable x.available)
        , ("enabled", makeNullableEncoder Jenc.bool x.enabled)
        , ("net", makeNullableEncoder Jenc.float x.net)
        , ("utilised", makeNullableEncoder (makeDictEncoder Jenc.float) x.utilised)
        ]

available : Jdec.Decoder Available
available =
    Jpipe.decode Available
        |> Jpipe.optional "adhoc_margin" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "cash" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "collateral" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "intraday_payin" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "live_balance" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "opening_balance" (Jdec.nullable Jdec.float) Nothing

encodeAvailable : Available -> Jenc.Value
encodeAvailable x =
    Jenc.object
        [ ("adhoc_margin", makeNullableEncoder Jenc.int x.adhocMargin)
        , ("cash", makeNullableEncoder Jenc.float x.cash)
        , ("collateral", makeNullableEncoder Jenc.int x.collateral)
        , ("intraday_payin", makeNullableEncoder Jenc.int x.intradayPayin)
        , ("live_balance", makeNullableEncoder Jenc.float x.liveBalance)
        , ("opening_balance", makeNullableEncoder Jenc.float x.openingBalance)
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

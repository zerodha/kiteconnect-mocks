-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import OrderMargins exposing (orderMargins)
--
-- and you're off to the races with
--
--     decodeString orderMargins myJsonString

module OrderMargins exposing
    ( OrderMargins
    , orderMarginsToString
    , orderMargins
    , Datum
    , Pnl
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias OrderMargins =
    { data : Maybe (Array Datum)
    , status : Maybe String
    }

type alias Datum =
    { additional : Maybe Int
    , bo : Maybe Int
    , cash : Maybe Int
    , exchange : Maybe String
    , exposure : Maybe Int
    , optionPremium : Maybe Int
    , pnl : Maybe Pnl
    , span : Maybe Int
    , total : Maybe Float
    , tradingsymbol : Maybe String
    , datumType : Maybe String
    , var : Maybe Float
    }

type alias Pnl =
    { realised : Maybe Int
    , unrealised : Maybe Int
    }

-- decoders and encoders

orderMarginsToString : OrderMargins -> String
orderMarginsToString r = Jenc.encode 0 (encodeOrderMargins r)

orderMargins : Jdec.Decoder OrderMargins
orderMargins =
    Jpipe.decode OrderMargins
        |> Jpipe.optional "data" (Jdec.nullable (Jdec.array datum)) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeOrderMargins : OrderMargins -> Jenc.Value
encodeOrderMargins x =
    Jenc.object
        [ ("data", makeNullableEncoder (makeArrayEncoder encodeDatum) x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
        ]

datum : Jdec.Decoder Datum
datum =
    Jpipe.decode Datum
        |> Jpipe.optional "additional" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "bo" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "cash" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "exchange" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "exposure" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "option_premium" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "pnl" (Jdec.nullable pnl) Nothing
        |> Jpipe.optional "span" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "total" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "tradingsymbol" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "type" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "var" (Jdec.nullable Jdec.float) Nothing

encodeDatum : Datum -> Jenc.Value
encodeDatum x =
    Jenc.object
        [ ("additional", makeNullableEncoder Jenc.int x.additional)
        , ("bo", makeNullableEncoder Jenc.int x.bo)
        , ("cash", makeNullableEncoder Jenc.int x.cash)
        , ("exchange", makeNullableEncoder Jenc.string x.exchange)
        , ("exposure", makeNullableEncoder Jenc.int x.exposure)
        , ("option_premium", makeNullableEncoder Jenc.int x.optionPremium)
        , ("pnl", makeNullableEncoder encodePnl x.pnl)
        , ("span", makeNullableEncoder Jenc.int x.span)
        , ("total", makeNullableEncoder Jenc.float x.total)
        , ("tradingsymbol", makeNullableEncoder Jenc.string x.tradingsymbol)
        , ("type", makeNullableEncoder Jenc.string x.datumType)
        , ("var", makeNullableEncoder Jenc.float x.var)
        ]

pnl : Jdec.Decoder Pnl
pnl =
    Jpipe.decode Pnl
        |> Jpipe.optional "realised" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "unrealised" (Jdec.nullable Jdec.int) Nothing

encodePnl : Pnl -> Jenc.Value
encodePnl x =
    Jenc.object
        [ ("realised", makeNullableEncoder Jenc.int x.realised)
        , ("unrealised", makeNullableEncoder Jenc.int x.unrealised)
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

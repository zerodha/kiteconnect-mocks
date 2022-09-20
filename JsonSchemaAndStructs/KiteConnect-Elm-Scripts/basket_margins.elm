-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import BasketMargins exposing (basketMargins)
--
-- and you're off to the races with
--
--     decodeString basketMargins myJsonString

module BasketMargins exposing
    ( BasketMargins
    , basketMarginsToString
    , basketMargins
    , Data
    , Final
    , Pnl
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias BasketMargins =
    { data : Maybe Data
    , status : Maybe String
    }

type alias Data =
    { final : Maybe Final
    , initial : Maybe Final
    , orders : Maybe (Array Final)
    }

type alias Final =
    { additional : Maybe Int
    , bo : Maybe Int
    , cash : Maybe Int
    , exchange : Maybe String
    , exposure : Maybe Float
    , optionPremium : Maybe Float
    , pnl : Maybe Pnl
    , span : Maybe Float
    , total : Maybe Float
    , tradingsymbol : Maybe String
    , finalType : Maybe String
    , var : Maybe Int
    }

type alias Pnl =
    { realised : Maybe Int
    , unrealised : Maybe Int
    }

-- decoders and encoders

basketMarginsToString : BasketMargins -> String
basketMarginsToString r = Jenc.encode 0 (encodeBasketMargins r)

basketMargins : Jdec.Decoder BasketMargins
basketMargins =
    Jpipe.decode BasketMargins
        |> Jpipe.optional "data" (Jdec.nullable data) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeBasketMargins : BasketMargins -> Jenc.Value
encodeBasketMargins x =
    Jenc.object
        [ ("data", makeNullableEncoder encodeData x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
        ]

data : Jdec.Decoder Data
data =
    Jpipe.decode Data
        |> Jpipe.optional "final" (Jdec.nullable final) Nothing
        |> Jpipe.optional "initial" (Jdec.nullable final) Nothing
        |> Jpipe.optional "orders" (Jdec.nullable (Jdec.array final)) Nothing

encodeData : Data -> Jenc.Value
encodeData x =
    Jenc.object
        [ ("final", makeNullableEncoder encodeFinal x.final)
        , ("initial", makeNullableEncoder encodeFinal x.initial)
        , ("orders", makeNullableEncoder (makeArrayEncoder encodeFinal) x.orders)
        ]

final : Jdec.Decoder Final
final =
    Jpipe.decode Final
        |> Jpipe.optional "additional" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "bo" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "cash" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "exchange" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "exposure" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "option_premium" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "pnl" (Jdec.nullable pnl) Nothing
        |> Jpipe.optional "span" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "total" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "tradingsymbol" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "type" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "var" (Jdec.nullable Jdec.int) Nothing

encodeFinal : Final -> Jenc.Value
encodeFinal x =
    Jenc.object
        [ ("additional", makeNullableEncoder Jenc.int x.additional)
        , ("bo", makeNullableEncoder Jenc.int x.bo)
        , ("cash", makeNullableEncoder Jenc.int x.cash)
        , ("exchange", makeNullableEncoder Jenc.string x.exchange)
        , ("exposure", makeNullableEncoder Jenc.float x.exposure)
        , ("option_premium", makeNullableEncoder Jenc.float x.optionPremium)
        , ("pnl", makeNullableEncoder encodePnl x.pnl)
        , ("span", makeNullableEncoder Jenc.float x.span)
        , ("total", makeNullableEncoder Jenc.float x.total)
        , ("tradingsymbol", makeNullableEncoder Jenc.string x.tradingsymbol)
        , ("type", makeNullableEncoder Jenc.string x.finalType)
        , ("var", makeNullableEncoder Jenc.int x.var)
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

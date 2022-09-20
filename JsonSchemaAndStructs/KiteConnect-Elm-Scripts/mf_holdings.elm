-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import MfHoldings exposing (mfHoldings)
--
-- and you're off to the races with
--
--     decodeString mfHoldings myJsonString

module MfHoldings exposing
    ( MFHoldings
    , mfHoldingsToString
    , mfHoldings
    , Datum
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias MFHoldings =
    { data : Maybe (Array Datum)
    , status : Maybe String
    }

type alias Datum =
    { averagePrice : Maybe Float
    , folio : Maybe String
    , fund : Maybe String
    , lastPrice : Maybe Float
    , lastPriceDate : Maybe String
    , pledgedQuantity : Maybe Int
    , pnl : Maybe Int
    , quantity : Maybe Float
    , tradingsymbol : Maybe String
    }

-- decoders and encoders

mfHoldingsToString : MFHoldings -> String
mfHoldingsToString r = Jenc.encode 0 (encodeMFHoldings r)

mfHoldings : Jdec.Decoder MFHoldings
mfHoldings =
    Jpipe.decode MFHoldings
        |> Jpipe.optional "data" (Jdec.nullable (Jdec.array datum)) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeMFHoldings : MFHoldings -> Jenc.Value
encodeMFHoldings x =
    Jenc.object
        [ ("data", makeNullableEncoder (makeArrayEncoder encodeDatum) x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
        ]

datum : Jdec.Decoder Datum
datum =
    Jpipe.decode Datum
        |> Jpipe.optional "average_price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "folio" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "fund" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "last_price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "last_price_date" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "pledged_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "pnl" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "quantity" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "tradingsymbol" (Jdec.nullable Jdec.string) Nothing

encodeDatum : Datum -> Jenc.Value
encodeDatum x =
    Jenc.object
        [ ("average_price", makeNullableEncoder Jenc.float x.averagePrice)
        , ("folio", makeNullableEncoder Jenc.string x.folio)
        , ("fund", makeNullableEncoder Jenc.string x.fund)
        , ("last_price", makeNullableEncoder Jenc.float x.lastPrice)
        , ("last_price_date", makeNullableEncoder Jenc.string x.lastPriceDate)
        , ("pledged_quantity", makeNullableEncoder Jenc.int x.pledgedQuantity)
        , ("pnl", makeNullableEncoder Jenc.int x.pnl)
        , ("quantity", makeNullableEncoder Jenc.float x.quantity)
        , ("tradingsymbol", makeNullableEncoder Jenc.string x.tradingsymbol)
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

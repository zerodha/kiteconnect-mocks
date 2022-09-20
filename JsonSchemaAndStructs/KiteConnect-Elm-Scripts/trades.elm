-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import Trades exposing (trades)
--
-- and you're off to the races with
--
--     decodeString trades myJsonString

module Trades exposing
    ( Trades
    , tradesToString
    , trades
    , Datum
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias Trades =
    { data : Maybe (Array Datum)
    , status : Maybe String
    }

type alias Datum =
    { averagePrice : Maybe Float
    , exchange : Maybe String
    , exchangeOrderID : Maybe String
    , exchangeTimestamp : Maybe String
    , fillTimestamp : Maybe String
    , instrumentToken : Maybe Int
    , orderID : Maybe String
    , orderTimestamp : Maybe String
    , product : Maybe String
    , quantity : Maybe Int
    , tradeID : Maybe String
    , tradingsymbol : Maybe String
    , transactionType : Maybe String
    }

-- decoders and encoders

tradesToString : Trades -> String
tradesToString r = Jenc.encode 0 (encodeTrades r)

trades : Jdec.Decoder Trades
trades =
    Jpipe.decode Trades
        |> Jpipe.optional "data" (Jdec.nullable (Jdec.array datum)) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeTrades : Trades -> Jenc.Value
encodeTrades x =
    Jenc.object
        [ ("data", makeNullableEncoder (makeArrayEncoder encodeDatum) x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
        ]

datum : Jdec.Decoder Datum
datum =
    Jpipe.decode Datum
        |> Jpipe.optional "average_price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "exchange" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "exchange_order_id" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "exchange_timestamp" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "fill_timestamp" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "instrument_token" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "order_id" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "order_timestamp" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "product" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "trade_id" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "tradingsymbol" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "transaction_type" (Jdec.nullable Jdec.string) Nothing

encodeDatum : Datum -> Jenc.Value
encodeDatum x =
    Jenc.object
        [ ("average_price", makeNullableEncoder Jenc.float x.averagePrice)
        , ("exchange", makeNullableEncoder Jenc.string x.exchange)
        , ("exchange_order_id", makeNullableEncoder Jenc.string x.exchangeOrderID)
        , ("exchange_timestamp", makeNullableEncoder Jenc.string x.exchangeTimestamp)
        , ("fill_timestamp", makeNullableEncoder Jenc.string x.fillTimestamp)
        , ("instrument_token", makeNullableEncoder Jenc.int x.instrumentToken)
        , ("order_id", makeNullableEncoder Jenc.string x.orderID)
        , ("order_timestamp", makeNullableEncoder Jenc.string x.orderTimestamp)
        , ("product", makeNullableEncoder Jenc.string x.product)
        , ("quantity", makeNullableEncoder Jenc.int x.quantity)
        , ("trade_id", makeNullableEncoder Jenc.string x.tradeID)
        , ("tradingsymbol", makeNullableEncoder Jenc.string x.tradingsymbol)
        , ("transaction_type", makeNullableEncoder Jenc.string x.transactionType)
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

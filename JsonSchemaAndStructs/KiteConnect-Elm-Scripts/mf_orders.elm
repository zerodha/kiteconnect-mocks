-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import MfOrders exposing (mfOrders)
--
-- and you're off to the races with
--
--     decodeString mfOrders myJsonString

module MfOrders exposing
    ( MFOrders
    , mfOrdersToString
    , mfOrders
    , Datum
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias MFOrders =
    { data : Maybe (Array Datum)
    , status : Maybe String
    }

type alias Datum =
    { amount : Maybe Int
    , averagePrice : Maybe Int
    , exchangeOrderID : Maybe String
    , exchangeTimestamp : Maybe String
    , folio : Maybe ()
    , fund : Maybe String
    , lastPrice : Maybe Float
    , lastPriceDate : Maybe String
    , orderID : Maybe String
    , orderTimestamp : Maybe String
    , placedBy : Maybe String
    , purchaseType : Maybe String
    , quantity : Maybe Int
    , settlementID : Maybe String
    , status : Maybe String
    , statusMessage : Maybe String
    , tag : Maybe String
    , tradingsymbol : Maybe String
    , transactionType : Maybe String
    , variety : Maybe String
    }

-- decoders and encoders

mfOrdersToString : MFOrders -> String
mfOrdersToString r = Jenc.encode 0 (encodeMFOrders r)

mfOrders : Jdec.Decoder MFOrders
mfOrders =
    Jpipe.decode MFOrders
        |> Jpipe.optional "data" (Jdec.nullable (Jdec.array datum)) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeMFOrders : MFOrders -> Jenc.Value
encodeMFOrders x =
    Jenc.object
        [ ("data", makeNullableEncoder (makeArrayEncoder encodeDatum) x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
        ]

datum : Jdec.Decoder Datum
datum =
    Jpipe.decode Datum
        |> Jpipe.optional "amount" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "average_price" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "exchange_order_id" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "exchange_timestamp" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "folio" (Jdec.nullable (Jdec.null ())) Nothing
        |> Jpipe.optional "fund" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "last_price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "last_price_date" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "order_id" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "order_timestamp" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "placed_by" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "purchase_type" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "settlement_id" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "status_message" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "tag" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "tradingsymbol" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "transaction_type" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "variety" (Jdec.nullable Jdec.string) Nothing

encodeDatum : Datum -> Jenc.Value
encodeDatum x =
    Jenc.object
        [ ("amount", makeNullableEncoder Jenc.int x.amount)
        , ("average_price", makeNullableEncoder Jenc.int x.averagePrice)
        , ("exchange_order_id", makeNullableEncoder Jenc.string x.exchangeOrderID)
        , ("exchange_timestamp", makeNullableEncoder Jenc.string x.exchangeTimestamp)
        , ("folio", makeNullableEncoder (always Jenc.null) x.folio)
        , ("fund", makeNullableEncoder Jenc.string x.fund)
        , ("last_price", makeNullableEncoder Jenc.float x.lastPrice)
        , ("last_price_date", makeNullableEncoder Jenc.string x.lastPriceDate)
        , ("order_id", makeNullableEncoder Jenc.string x.orderID)
        , ("order_timestamp", makeNullableEncoder Jenc.string x.orderTimestamp)
        , ("placed_by", makeNullableEncoder Jenc.string x.placedBy)
        , ("purchase_type", makeNullableEncoder Jenc.string x.purchaseType)
        , ("quantity", makeNullableEncoder Jenc.int x.quantity)
        , ("settlement_id", makeNullableEncoder Jenc.string x.settlementID)
        , ("status", makeNullableEncoder Jenc.string x.status)
        , ("status_message", makeNullableEncoder Jenc.string x.statusMessage)
        , ("tag", makeNullableEncoder Jenc.string x.tag)
        , ("tradingsymbol", makeNullableEncoder Jenc.string x.tradingsymbol)
        , ("transaction_type", makeNullableEncoder Jenc.string x.transactionType)
        , ("variety", makeNullableEncoder Jenc.string x.variety)
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

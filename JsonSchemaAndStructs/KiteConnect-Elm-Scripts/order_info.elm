-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import OrderInfo exposing (orderInfo)
--
-- and you're off to the races with
--
--     decodeString orderInfo myJsonString

module OrderInfo exposing
    ( OrderInfo
    , orderInfoToString
    , orderInfo
    , Datum
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias OrderInfo =
    { data : Maybe (Array Datum)
    , status : Maybe String
    }

type alias Datum =
    { averagePrice : Maybe Int
    , cancelledQuantity : Maybe Int
    , disclosedQuantity : Maybe Int
    , exchange : Maybe String
    , exchangeOrderID : Maybe String
    , exchangeTimestamp : Maybe String
    , filledQuantity : Maybe Int
    , instrumentToken : Maybe Int
    , orderID : Maybe String
    , orderTimestamp : Maybe String
    , orderType : Maybe String
    , parentOrderID : Maybe ()
    , pendingQuantity : Maybe Int
    , placedBy : Maybe String
    , price : Maybe Float
    , product : Maybe String
    , quantity : Maybe Int
    , status : Maybe String
    , statusMessage : Maybe ()
    , tag : Maybe ()
    , tradingsymbol : Maybe String
    , transactionType : Maybe String
    , triggerPrice : Maybe Int
    , validity : Maybe String
    , variety : Maybe String
    }

-- decoders and encoders

orderInfoToString : OrderInfo -> String
orderInfoToString r = Jenc.encode 0 (encodeOrderInfo r)

orderInfo : Jdec.Decoder OrderInfo
orderInfo =
    Jpipe.decode OrderInfo
        |> Jpipe.optional "data" (Jdec.nullable (Jdec.array datum)) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeOrderInfo : OrderInfo -> Jenc.Value
encodeOrderInfo x =
    Jenc.object
        [ ("data", makeNullableEncoder (makeArrayEncoder encodeDatum) x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
        ]

datum : Jdec.Decoder Datum
datum =
    Jpipe.decode Datum
        |> Jpipe.optional "average_price" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "cancelled_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "disclosed_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "exchange" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "exchange_order_id" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "exchange_timestamp" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "filled_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "instrument_token" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "order_id" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "order_timestamp" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "order_type" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "parent_order_id" (Jdec.nullable (Jdec.null ())) Nothing
        |> Jpipe.optional "pending_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "placed_by" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "product" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "status_message" (Jdec.nullable (Jdec.null ())) Nothing
        |> Jpipe.optional "tag" (Jdec.nullable (Jdec.null ())) Nothing
        |> Jpipe.optional "tradingsymbol" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "transaction_type" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "trigger_price" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "validity" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "variety" (Jdec.nullable Jdec.string) Nothing

encodeDatum : Datum -> Jenc.Value
encodeDatum x =
    Jenc.object
        [ ("average_price", makeNullableEncoder Jenc.int x.averagePrice)
        , ("cancelled_quantity", makeNullableEncoder Jenc.int x.cancelledQuantity)
        , ("disclosed_quantity", makeNullableEncoder Jenc.int x.disclosedQuantity)
        , ("exchange", makeNullableEncoder Jenc.string x.exchange)
        , ("exchange_order_id", makeNullableEncoder Jenc.string x.exchangeOrderID)
        , ("exchange_timestamp", makeNullableEncoder Jenc.string x.exchangeTimestamp)
        , ("filled_quantity", makeNullableEncoder Jenc.int x.filledQuantity)
        , ("instrument_token", makeNullableEncoder Jenc.int x.instrumentToken)
        , ("order_id", makeNullableEncoder Jenc.string x.orderID)
        , ("order_timestamp", makeNullableEncoder Jenc.string x.orderTimestamp)
        , ("order_type", makeNullableEncoder Jenc.string x.orderType)
        , ("parent_order_id", makeNullableEncoder (always Jenc.null) x.parentOrderID)
        , ("pending_quantity", makeNullableEncoder Jenc.int x.pendingQuantity)
        , ("placed_by", makeNullableEncoder Jenc.string x.placedBy)
        , ("price", makeNullableEncoder Jenc.float x.price)
        , ("product", makeNullableEncoder Jenc.string x.product)
        , ("quantity", makeNullableEncoder Jenc.int x.quantity)
        , ("status", makeNullableEncoder Jenc.string x.status)
        , ("status_message", makeNullableEncoder (always Jenc.null) x.statusMessage)
        , ("tag", makeNullableEncoder (always Jenc.null) x.tag)
        , ("tradingsymbol", makeNullableEncoder Jenc.string x.tradingsymbol)
        , ("transaction_type", makeNullableEncoder Jenc.string x.transactionType)
        , ("trigger_price", makeNullableEncoder Jenc.int x.triggerPrice)
        , ("validity", makeNullableEncoder Jenc.string x.validity)
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

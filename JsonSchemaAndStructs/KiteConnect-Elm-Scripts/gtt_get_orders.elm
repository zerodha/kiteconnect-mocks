-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import GttGetOrders exposing (gttGetOrders)
--
-- and you're off to the races with
--
--     decodeString gttGetOrders myJsonString

module GttGetOrders exposing
    ( GttGetOrders
    , gttGetOrdersToString
    , gttGetOrders
    , Datum
    , Condition
    , Meta
    , Order
    , Result
    , OrderResult
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias GttGetOrders =
    { data : Maybe (Array Datum)
    , status : Maybe String
    }

type alias Datum =
    { condition : Maybe Condition
    , createdAt : Maybe String
    , expiresAt : Maybe String
    , id : Maybe Int
    , meta : Maybe Meta
    , orders : Maybe (Array Order)
    , parentTrigger : Maybe ()
    , status : Maybe String
    , datumType : Maybe String
    , updatedAt : Maybe String
    , userID : Maybe String
    }

type alias Condition =
    { exchange : Maybe String
    , instrumentToken : Maybe Int
    , lastPrice : Maybe Float
    , tradingsymbol : Maybe String
    , triggerValues : Maybe (Array Float)
    }

type alias Meta =
    {
    }

type alias Order =
    { exchange : Maybe String
    , orderType : Maybe String
    , price : Maybe Float
    , product : Maybe String
    , quantity : Maybe Int
    , result : Maybe Result
    , tradingsymbol : Maybe String
    , transactionType : Maybe String
    }

type alias Result =
    { accountID : Maybe String
    , exchange : Maybe String
    , meta : Maybe String
    , orderResult : Maybe OrderResult
    , orderType : Maybe String
    , price : Maybe Int
    , product : Maybe String
    , quantity : Maybe Int
    , timestamp : Maybe String
    , tradingsymbol : Maybe String
    , transactionType : Maybe String
    , triggeredAt : Maybe Float
    , validity : Maybe String
    }

type alias OrderResult =
    { orderID : Maybe String
    , rejectionReason : Maybe String
    , status : Maybe String
    }

-- decoders and encoders

gttGetOrdersToString : GttGetOrders -> String
gttGetOrdersToString r = Jenc.encode 0 (encodeGttGetOrders r)

gttGetOrders : Jdec.Decoder GttGetOrders
gttGetOrders =
    Jpipe.decode GttGetOrders
        |> Jpipe.optional "data" (Jdec.nullable (Jdec.array datum)) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeGttGetOrders : GttGetOrders -> Jenc.Value
encodeGttGetOrders x =
    Jenc.object
        [ ("data", makeNullableEncoder (makeArrayEncoder encodeDatum) x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
        ]

datum : Jdec.Decoder Datum
datum =
    Jpipe.decode Datum
        |> Jpipe.optional "condition" (Jdec.nullable condition) Nothing
        |> Jpipe.optional "created_at" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "expires_at" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "id" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "meta" (Jdec.nullable meta) Nothing
        |> Jpipe.optional "orders" (Jdec.nullable (Jdec.array order)) Nothing
        |> Jpipe.optional "parent_trigger" (Jdec.nullable (Jdec.null ())) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "type" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "updated_at" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "user_id" (Jdec.nullable Jdec.string) Nothing

encodeDatum : Datum -> Jenc.Value
encodeDatum x =
    Jenc.object
        [ ("condition", makeNullableEncoder encodeCondition x.condition)
        , ("created_at", makeNullableEncoder Jenc.string x.createdAt)
        , ("expires_at", makeNullableEncoder Jenc.string x.expiresAt)
        , ("id", makeNullableEncoder Jenc.int x.id)
        , ("meta", makeNullableEncoder encodeMeta x.meta)
        , ("orders", makeNullableEncoder (makeArrayEncoder encodeOrder) x.orders)
        , ("parent_trigger", makeNullableEncoder (always Jenc.null) x.parentTrigger)
        , ("status", makeNullableEncoder Jenc.string x.status)
        , ("type", makeNullableEncoder Jenc.string x.datumType)
        , ("updated_at", makeNullableEncoder Jenc.string x.updatedAt)
        , ("user_id", makeNullableEncoder Jenc.string x.userID)
        ]

condition : Jdec.Decoder Condition
condition =
    Jpipe.decode Condition
        |> Jpipe.optional "exchange" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "instrument_token" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "last_price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "tradingsymbol" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "trigger_values" (Jdec.nullable (Jdec.array Jdec.float)) Nothing

encodeCondition : Condition -> Jenc.Value
encodeCondition x =
    Jenc.object
        [ ("exchange", makeNullableEncoder Jenc.string x.exchange)
        , ("instrument_token", makeNullableEncoder Jenc.int x.instrumentToken)
        , ("last_price", makeNullableEncoder Jenc.float x.lastPrice)
        , ("tradingsymbol", makeNullableEncoder Jenc.string x.tradingsymbol)
        , ("trigger_values", makeNullableEncoder (makeArrayEncoder Jenc.float) x.triggerValues)
        ]

meta : Jdec.Decoder Meta
meta =
    Jpipe.decode Meta

encodeMeta : Meta -> Jenc.Value
encodeMeta x =
    Jenc.object
        [
        ]

order : Jdec.Decoder Order
order =
    Jpipe.decode Order
        |> Jpipe.optional "exchange" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "order_type" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "product" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "result" (Jdec.nullable result) Nothing
        |> Jpipe.optional "tradingsymbol" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "transaction_type" (Jdec.nullable Jdec.string) Nothing

encodeOrder : Order -> Jenc.Value
encodeOrder x =
    Jenc.object
        [ ("exchange", makeNullableEncoder Jenc.string x.exchange)
        , ("order_type", makeNullableEncoder Jenc.string x.orderType)
        , ("price", makeNullableEncoder Jenc.float x.price)
        , ("product", makeNullableEncoder Jenc.string x.product)
        , ("quantity", makeNullableEncoder Jenc.int x.quantity)
        , ("result", makeNullableEncoder encodeResult x.result)
        , ("tradingsymbol", makeNullableEncoder Jenc.string x.tradingsymbol)
        , ("transaction_type", makeNullableEncoder Jenc.string x.transactionType)
        ]

result : Jdec.Decoder Result
result =
    Jpipe.decode Result
        |> Jpipe.optional "account_id" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "exchange" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "meta" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "order_result" (Jdec.nullable orderResult) Nothing
        |> Jpipe.optional "order_type" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "price" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "product" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "timestamp" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "tradingsymbol" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "transaction_type" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "triggered_at" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "validity" (Jdec.nullable Jdec.string) Nothing

encodeResult : Result -> Jenc.Value
encodeResult x =
    Jenc.object
        [ ("account_id", makeNullableEncoder Jenc.string x.accountID)
        , ("exchange", makeNullableEncoder Jenc.string x.exchange)
        , ("meta", makeNullableEncoder Jenc.string x.meta)
        , ("order_result", makeNullableEncoder encodeOrderResult x.orderResult)
        , ("order_type", makeNullableEncoder Jenc.string x.orderType)
        , ("price", makeNullableEncoder Jenc.int x.price)
        , ("product", makeNullableEncoder Jenc.string x.product)
        , ("quantity", makeNullableEncoder Jenc.int x.quantity)
        , ("timestamp", makeNullableEncoder Jenc.string x.timestamp)
        , ("tradingsymbol", makeNullableEncoder Jenc.string x.tradingsymbol)
        , ("transaction_type", makeNullableEncoder Jenc.string x.transactionType)
        , ("triggered_at", makeNullableEncoder Jenc.float x.triggeredAt)
        , ("validity", makeNullableEncoder Jenc.string x.validity)
        ]

orderResult : Jdec.Decoder OrderResult
orderResult =
    Jpipe.decode OrderResult
        |> Jpipe.optional "order_id" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "rejection_reason" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeOrderResult : OrderResult -> Jenc.Value
encodeOrderResult x =
    Jenc.object
        [ ("order_id", makeNullableEncoder Jenc.string x.orderID)
        , ("rejection_reason", makeNullableEncoder Jenc.string x.rejectionReason)
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

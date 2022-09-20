-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import Orders exposing (orders)
--
-- and you're off to the races with
--
--     decodeString orders myJsonString

module Orders exposing
    ( Orders
    , ordersToString
    , orders
    , Datum
    , Meta
    , Iceberg
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias Orders =
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
    , exchangeUpdateTimestamp : Maybe String
    , filledQuantity : Maybe Int
    , guid : Maybe String
    , instrumentToken : Maybe Int
    , marketProtection : Maybe Int
    , meta : Maybe Meta
    , modified : Maybe Bool
    , orderID : Maybe String
    , orderTimestamp : Maybe String
    , orderType : Maybe String
    , parentOrderID : Maybe ()
    , pendingQuantity : Maybe Int
    , placedBy : Maybe String
    , price : Maybe Int
    , product : Maybe String
    , quantity : Maybe Int
    , status : Maybe String
    , statusMessage : Maybe String
    , statusMessageRaw : Maybe String
    , tag : Maybe String
    , tags : Maybe (Array String)
    , tradingsymbol : Maybe String
    , transactionType : Maybe String
    , triggerPrice : Maybe Int
    , validity : Maybe String
    , validityTTL : Maybe Int
    , variety : Maybe String
    }

type alias Meta =
    { iceberg : Maybe Iceberg
    }

type alias Iceberg =
    { leg : Maybe Int
    , legQuantity : Maybe Int
    , legs : Maybe Int
    , remainingQuantity : Maybe Int
    , totalQuantity : Maybe Int
    }

-- decoders and encoders

ordersToString : Orders -> String
ordersToString r = Jenc.encode 0 (encodeOrders r)

orders : Jdec.Decoder Orders
orders =
    Jpipe.decode Orders
        |> Jpipe.optional "data" (Jdec.nullable (Jdec.array datum)) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeOrders : Orders -> Jenc.Value
encodeOrders x =
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
        |> Jpipe.optional "exchange_update_timestamp" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "filled_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "guid" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "instrument_token" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "market_protection" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "meta" (Jdec.nullable meta) Nothing
        |> Jpipe.optional "modified" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "order_id" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "order_timestamp" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "order_type" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "parent_order_id" (Jdec.nullable (Jdec.null ())) Nothing
        |> Jpipe.optional "pending_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "placed_by" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "price" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "product" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "status_message" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "status_message_raw" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "tag" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "tags" (Jdec.nullable (Jdec.array Jdec.string)) Nothing
        |> Jpipe.optional "tradingsymbol" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "transaction_type" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "trigger_price" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "validity" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "validity_ttl" (Jdec.nullable Jdec.int) Nothing
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
        , ("exchange_update_timestamp", makeNullableEncoder Jenc.string x.exchangeUpdateTimestamp)
        , ("filled_quantity", makeNullableEncoder Jenc.int x.filledQuantity)
        , ("guid", makeNullableEncoder Jenc.string x.guid)
        , ("instrument_token", makeNullableEncoder Jenc.int x.instrumentToken)
        , ("market_protection", makeNullableEncoder Jenc.int x.marketProtection)
        , ("meta", makeNullableEncoder encodeMeta x.meta)
        , ("modified", makeNullableEncoder Jenc.bool x.modified)
        , ("order_id", makeNullableEncoder Jenc.string x.orderID)
        , ("order_timestamp", makeNullableEncoder Jenc.string x.orderTimestamp)
        , ("order_type", makeNullableEncoder Jenc.string x.orderType)
        , ("parent_order_id", makeNullableEncoder (always Jenc.null) x.parentOrderID)
        , ("pending_quantity", makeNullableEncoder Jenc.int x.pendingQuantity)
        , ("placed_by", makeNullableEncoder Jenc.string x.placedBy)
        , ("price", makeNullableEncoder Jenc.int x.price)
        , ("product", makeNullableEncoder Jenc.string x.product)
        , ("quantity", makeNullableEncoder Jenc.int x.quantity)
        , ("status", makeNullableEncoder Jenc.string x.status)
        , ("status_message", makeNullableEncoder Jenc.string x.statusMessage)
        , ("status_message_raw", makeNullableEncoder Jenc.string x.statusMessageRaw)
        , ("tag", makeNullableEncoder Jenc.string x.tag)
        , ("tags", makeNullableEncoder (makeArrayEncoder Jenc.string) x.tags)
        , ("tradingsymbol", makeNullableEncoder Jenc.string x.tradingsymbol)
        , ("transaction_type", makeNullableEncoder Jenc.string x.transactionType)
        , ("trigger_price", makeNullableEncoder Jenc.int x.triggerPrice)
        , ("validity", makeNullableEncoder Jenc.string x.validity)
        , ("validity_ttl", makeNullableEncoder Jenc.int x.validityTTL)
        , ("variety", makeNullableEncoder Jenc.string x.variety)
        ]

meta : Jdec.Decoder Meta
meta =
    Jpipe.decode Meta
        |> Jpipe.optional "iceberg" (Jdec.nullable iceberg) Nothing

encodeMeta : Meta -> Jenc.Value
encodeMeta x =
    Jenc.object
        [ ("iceberg", makeNullableEncoder encodeIceberg x.iceberg)
        ]

iceberg : Jdec.Decoder Iceberg
iceberg =
    Jpipe.decode Iceberg
        |> Jpipe.optional "leg" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "leg_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "legs" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "remaining_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "total_quantity" (Jdec.nullable Jdec.int) Nothing

encodeIceberg : Iceberg -> Jenc.Value
encodeIceberg x =
    Jenc.object
        [ ("leg", makeNullableEncoder Jenc.int x.leg)
        , ("leg_quantity", makeNullableEncoder Jenc.int x.legQuantity)
        , ("legs", makeNullableEncoder Jenc.int x.legs)
        , ("remaining_quantity", makeNullableEncoder Jenc.int x.remainingQuantity)
        , ("total_quantity", makeNullableEncoder Jenc.int x.totalQuantity)
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

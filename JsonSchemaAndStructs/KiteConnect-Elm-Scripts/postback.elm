-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import Postback exposing (postback)
--
-- and you're off to the races with
--
--     decodeString postback myJsonString

module Postback exposing
    ( Postback
    , postbackToString
    , postback
    , Meta
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias Postback =
    { appID : Maybe Int
    , averagePrice : Maybe Int
    , cancelledQuantity : Maybe Int
    , checksum : Maybe String
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
    , statusMessage : Maybe ()
    , statusMessageRaw : Maybe ()
    , tag : Maybe ()
    , tradingsymbol : Maybe String
    , transactionType : Maybe String
    , triggerPrice : Maybe Int
    , unfilledQuantity : Maybe Int
    , userID : Maybe String
    , validity : Maybe String
    , variety : Maybe String
    }

type alias Meta =
    {
    }

-- decoders and encoders

postbackToString : Postback -> String
postbackToString r = Jenc.encode 0 (encodePostback r)

postback : Jdec.Decoder Postback
postback =
    Jpipe.decode Postback
        |> Jpipe.optional "app_id" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "average_price" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "cancelled_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "checksum" (Jdec.nullable Jdec.string) Nothing
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
        |> Jpipe.optional "status_message" (Jdec.nullable (Jdec.null ())) Nothing
        |> Jpipe.optional "status_message_raw" (Jdec.nullable (Jdec.null ())) Nothing
        |> Jpipe.optional "tag" (Jdec.nullable (Jdec.null ())) Nothing
        |> Jpipe.optional "tradingsymbol" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "transaction_type" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "trigger_price" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "unfilled_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "user_id" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "validity" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "variety" (Jdec.nullable Jdec.string) Nothing

encodePostback : Postback -> Jenc.Value
encodePostback x =
    Jenc.object
        [ ("app_id", makeNullableEncoder Jenc.int x.appID)
        , ("average_price", makeNullableEncoder Jenc.int x.averagePrice)
        , ("cancelled_quantity", makeNullableEncoder Jenc.int x.cancelledQuantity)
        , ("checksum", makeNullableEncoder Jenc.string x.checksum)
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
        , ("status_message", makeNullableEncoder (always Jenc.null) x.statusMessage)
        , ("status_message_raw", makeNullableEncoder (always Jenc.null) x.statusMessageRaw)
        , ("tag", makeNullableEncoder (always Jenc.null) x.tag)
        , ("tradingsymbol", makeNullableEncoder Jenc.string x.tradingsymbol)
        , ("transaction_type", makeNullableEncoder Jenc.string x.transactionType)
        , ("trigger_price", makeNullableEncoder Jenc.int x.triggerPrice)
        , ("unfilled_quantity", makeNullableEncoder Jenc.int x.unfilledQuantity)
        , ("user_id", makeNullableEncoder Jenc.string x.userID)
        , ("validity", makeNullableEncoder Jenc.string x.validity)
        , ("variety", makeNullableEncoder Jenc.string x.variety)
        ]

meta : Jdec.Decoder Meta
meta =
    Jpipe.decode Meta

encodeMeta : Meta -> Jenc.Value
encodeMeta x =
    Jenc.object
        [
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

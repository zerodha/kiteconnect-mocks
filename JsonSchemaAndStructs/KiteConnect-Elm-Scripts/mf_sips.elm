-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import MfSips exposing (mfSips)
--
-- and you're off to the races with
--
--     decodeString mfSips myJsonString

module MfSips exposing
    ( MFSips
    , mfSipsToString
    , mfSips
    , Datum
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias MFSips =
    { data : Maybe (Array Datum)
    }

type alias Datum =
    { completedInstalments : Maybe Int
    , created : Maybe String
    , dividendType : Maybe String
    , frequency : Maybe String
    , fund : Maybe String
    , instalmentAmount : Maybe Int
    , instalmentDay : Maybe Int
    , instalments : Maybe Int
    , lastInstalment : Maybe String
    , nextInstalment : Maybe String
    , pendingInstalments : Maybe Int
    , sipID : Maybe String
    , sipRegNum : Maybe String
    , sipType : Maybe String
    , status : Maybe String
    , stepUp : Maybe (Dict String Int)
    , tag : Maybe String
    , tradingsymbol : Maybe String
    , transactionType : Maybe String
    , triggerPrice : Maybe Int
    }

-- decoders and encoders

mfSipsToString : MFSips -> String
mfSipsToString r = Jenc.encode 0 (encodeMFSips r)

mfSips : Jdec.Decoder MFSips
mfSips =
    Jpipe.decode MFSips
        |> Jpipe.optional "data" (Jdec.nullable (Jdec.array datum)) Nothing

encodeMFSips : MFSips -> Jenc.Value
encodeMFSips x =
    Jenc.object
        [ ("data", makeNullableEncoder (makeArrayEncoder encodeDatum) x.data)
        ]

datum : Jdec.Decoder Datum
datum =
    Jpipe.decode Datum
        |> Jpipe.optional "completed_instalments" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "created" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "dividend_type" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "frequency" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "fund" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "instalment_amount" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "instalment_day" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "instalments" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "last_instalment" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "next_instalment" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "pending_instalments" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "sip_id" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "sip_reg_num" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "sip_type" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "step_up" (Jdec.nullable (Jdec.dict Jdec.int)) Nothing
        |> Jpipe.optional "tag" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "tradingsymbol" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "transaction_type" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "trigger_price" (Jdec.nullable Jdec.int) Nothing

encodeDatum : Datum -> Jenc.Value
encodeDatum x =
    Jenc.object
        [ ("completed_instalments", makeNullableEncoder Jenc.int x.completedInstalments)
        , ("created", makeNullableEncoder Jenc.string x.created)
        , ("dividend_type", makeNullableEncoder Jenc.string x.dividendType)
        , ("frequency", makeNullableEncoder Jenc.string x.frequency)
        , ("fund", makeNullableEncoder Jenc.string x.fund)
        , ("instalment_amount", makeNullableEncoder Jenc.int x.instalmentAmount)
        , ("instalment_day", makeNullableEncoder Jenc.int x.instalmentDay)
        , ("instalments", makeNullableEncoder Jenc.int x.instalments)
        , ("last_instalment", makeNullableEncoder Jenc.string x.lastInstalment)
        , ("next_instalment", makeNullableEncoder Jenc.string x.nextInstalment)
        , ("pending_instalments", makeNullableEncoder Jenc.int x.pendingInstalments)
        , ("sip_id", makeNullableEncoder Jenc.string x.sipID)
        , ("sip_reg_num", makeNullableEncoder Jenc.string x.sipRegNum)
        , ("sip_type", makeNullableEncoder Jenc.string x.sipType)
        , ("status", makeNullableEncoder Jenc.string x.status)
        , ("step_up", makeNullableEncoder (makeDictEncoder Jenc.int) x.stepUp)
        , ("tag", makeNullableEncoder Jenc.string x.tag)
        , ("tradingsymbol", makeNullableEncoder Jenc.string x.tradingsymbol)
        , ("transaction_type", makeNullableEncoder Jenc.string x.transactionType)
        , ("trigger_price", makeNullableEncoder Jenc.int x.triggerPrice)
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

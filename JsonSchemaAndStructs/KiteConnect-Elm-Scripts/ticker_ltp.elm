-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import TickerLtp exposing (tickerLtp)
--
-- and you're off to the races with
--
--     decodeString tickerLtp myJsonString

module TickerLtp exposing
    ( TickerLtp
    , tickerLtpToString
    , tickerLtp
    , TriggerRangeElement
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias TickerLtp = Array TriggerRangeElement

type alias TriggerRangeElement =
    { instrumentToken : Maybe Int
    , lastPrice : Maybe Int
    , mode : Maybe String
    , tradable : Maybe Bool
    }

-- decoders and encoders

tickerLtp : Jdec.Decoder TickerLtp
tickerLtp = Jdec.array triggerRangeElement

tickerLtpToString : TickerLtp -> String
tickerLtpToString r = Jenc.encode 0 (makeArrayEncoder encodeTriggerRangeElement r)

triggerRangeElement : Jdec.Decoder TriggerRangeElement
triggerRangeElement =
    Jpipe.decode TriggerRangeElement
        |> Jpipe.optional "instrument_token" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "last_price" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "mode" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "tradable" (Jdec.nullable Jdec.bool) Nothing

encodeTriggerRangeElement : TriggerRangeElement -> Jenc.Value
encodeTriggerRangeElement x =
    Jenc.object
        [ ("instrument_token", makeNullableEncoder Jenc.int x.instrumentToken)
        , ("last_price", makeNullableEncoder Jenc.int x.lastPrice)
        , ("mode", makeNullableEncoder Jenc.string x.mode)
        , ("tradable", makeNullableEncoder Jenc.bool x.tradable)
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

-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import InstrumentsNse exposing (instrumentsNse)
--
-- and you're off to the races with
--
--     decodeString instrumentsNse myJsonString

module InstrumentsNse exposing
    ( InstrumentsNse
    , instrumentsNseToString
    , instrumentsNse
    , InstrumentsNseElement
    , Exchange(..)
    , InstrumentType(..)
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias InstrumentsNse = Array InstrumentsNseElement

type alias InstrumentsNseElement =
    { exchange : Maybe Exchange
    , exchangeToken : Maybe Int
    , expiry : Maybe String
    , instrumentToken : Maybe Int
    , instrumentType : Maybe InstrumentType
    , lastPrice : Maybe Int
    , lotSize : Maybe Int
    , name : Maybe String
    , segment : Maybe Exchange
    , strike : Maybe Int
    , tickSize : Maybe Float
    , tradingsymbol : Maybe String
    }

type Exchange
    = Nse

type InstrumentType
    = Eq

-- decoders and encoders

instrumentsNse : Jdec.Decoder InstrumentsNse
instrumentsNse = Jdec.array instrumentsNseElement

instrumentsNseToString : InstrumentsNse -> String
instrumentsNseToString r = Jenc.encode 0 (makeArrayEncoder encodeInstrumentsNseElement r)

instrumentsNseElement : Jdec.Decoder InstrumentsNseElement
instrumentsNseElement =
    Jpipe.decode InstrumentsNseElement
        |> Jpipe.optional "exchange" (Jdec.nullable exchange) Nothing
        |> Jpipe.optional "exchange_token" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "expiry" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "instrument_token" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "instrument_type" (Jdec.nullable instrumentType) Nothing
        |> Jpipe.optional "last_price" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "lot_size" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "name" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "segment" (Jdec.nullable exchange) Nothing
        |> Jpipe.optional "strike" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "tick_size" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "tradingsymbol" (Jdec.nullable Jdec.string) Nothing

encodeInstrumentsNseElement : InstrumentsNseElement -> Jenc.Value
encodeInstrumentsNseElement x =
    Jenc.object
        [ ("exchange", makeNullableEncoder encodeExchange x.exchange)
        , ("exchange_token", makeNullableEncoder Jenc.int x.exchangeToken)
        , ("expiry", makeNullableEncoder Jenc.string x.expiry)
        , ("instrument_token", makeNullableEncoder Jenc.int x.instrumentToken)
        , ("instrument_type", makeNullableEncoder encodeInstrumentType x.instrumentType)
        , ("last_price", makeNullableEncoder Jenc.int x.lastPrice)
        , ("lot_size", makeNullableEncoder Jenc.int x.lotSize)
        , ("name", makeNullableEncoder Jenc.string x.name)
        , ("segment", makeNullableEncoder encodeExchange x.segment)
        , ("strike", makeNullableEncoder Jenc.int x.strike)
        , ("tick_size", makeNullableEncoder Jenc.float x.tickSize)
        , ("tradingsymbol", makeNullableEncoder Jenc.string x.tradingsymbol)
        ]

exchange : Jdec.Decoder Exchange
exchange =
    Jdec.string
        |> Jdec.andThen (\str ->
            case str of
                "NSE" -> Jdec.succeed Nse
                somethingElse -> Jdec.fail <| "Invalid Exchange: " ++ somethingElse
        )

encodeExchange : Exchange -> Jenc.Value
encodeExchange x = case x of
    Nse -> Jenc.string "NSE"

instrumentType : Jdec.Decoder InstrumentType
instrumentType =
    Jdec.string
        |> Jdec.andThen (\str ->
            case str of
                "EQ" -> Jdec.succeed Eq
                somethingElse -> Jdec.fail <| "Invalid InstrumentType: " ++ somethingElse
        )

encodeInstrumentType : InstrumentType -> Jenc.Value
encodeInstrumentType x = case x of
    Eq -> Jenc.string "EQ"

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

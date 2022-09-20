-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import InstrumentsAll exposing (instrumentsAll)
--
-- and you're off to the races with
--
--     decodeString instrumentsAll myJsonString

module InstrumentsAll exposing
    ( InstrumentsAll
    , instrumentsAllToString
    , instrumentsAll
    , InstrumentsAllElement
    , Exchange(..)
    , InstrumentType(..)
    , Segment(..)
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias InstrumentsAll = Array InstrumentsAllElement

type alias InstrumentsAllElement =
    { exchange : Maybe Exchange
    , exchangeToken : Maybe Int
    , expiry : Maybe String
    , instrumentToken : Maybe Int
    , instrumentType : Maybe InstrumentType
    , lastPrice : Maybe Float
    , lotSize : Maybe Int
    , name : Maybe String
    , segment : Maybe Segment
    , strike : Maybe Int
    , tickSize : Maybe Float
    , tradingsymbol : Maybe String
    }

type Exchange
    = ExchangeBSE
    | ExchangeNSE
    | Nfo

type InstrumentType
    = Ce
    | Eq
    | PE

type Segment
    = NfoOpt
    | SegmentBSE
    | SegmentNSE

-- decoders and encoders

instrumentsAll : Jdec.Decoder InstrumentsAll
instrumentsAll = Jdec.array instrumentsAllElement

instrumentsAllToString : InstrumentsAll -> String
instrumentsAllToString r = Jenc.encode 0 (makeArrayEncoder encodeInstrumentsAllElement r)

instrumentsAllElement : Jdec.Decoder InstrumentsAllElement
instrumentsAllElement =
    Jpipe.decode InstrumentsAllElement
        |> Jpipe.optional "exchange" (Jdec.nullable exchange) Nothing
        |> Jpipe.optional "exchange_token" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "expiry" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "instrument_token" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "instrument_type" (Jdec.nullable instrumentType) Nothing
        |> Jpipe.optional "last_price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "lot_size" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "name" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "segment" (Jdec.nullable segment) Nothing
        |> Jpipe.optional "strike" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "tick_size" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "tradingsymbol" (Jdec.nullable Jdec.string) Nothing

encodeInstrumentsAllElement : InstrumentsAllElement -> Jenc.Value
encodeInstrumentsAllElement x =
    Jenc.object
        [ ("exchange", makeNullableEncoder encodeExchange x.exchange)
        , ("exchange_token", makeNullableEncoder Jenc.int x.exchangeToken)
        , ("expiry", makeNullableEncoder Jenc.string x.expiry)
        , ("instrument_token", makeNullableEncoder Jenc.int x.instrumentToken)
        , ("instrument_type", makeNullableEncoder encodeInstrumentType x.instrumentType)
        , ("last_price", makeNullableEncoder Jenc.float x.lastPrice)
        , ("lot_size", makeNullableEncoder Jenc.int x.lotSize)
        , ("name", makeNullableEncoder Jenc.string x.name)
        , ("segment", makeNullableEncoder encodeSegment x.segment)
        , ("strike", makeNullableEncoder Jenc.int x.strike)
        , ("tick_size", makeNullableEncoder Jenc.float x.tickSize)
        , ("tradingsymbol", makeNullableEncoder Jenc.string x.tradingsymbol)
        ]

exchange : Jdec.Decoder Exchange
exchange =
    Jdec.string
        |> Jdec.andThen (\str ->
            case str of
                "BSE" -> Jdec.succeed ExchangeBSE
                "NSE" -> Jdec.succeed ExchangeNSE
                "NFO" -> Jdec.succeed Nfo
                somethingElse -> Jdec.fail <| "Invalid Exchange: " ++ somethingElse
        )

encodeExchange : Exchange -> Jenc.Value
encodeExchange x = case x of
    ExchangeBSE -> Jenc.string "BSE"
    ExchangeNSE -> Jenc.string "NSE"
    Nfo -> Jenc.string "NFO"

instrumentType : Jdec.Decoder InstrumentType
instrumentType =
    Jdec.string
        |> Jdec.andThen (\str ->
            case str of
                "CE" -> Jdec.succeed Ce
                "EQ" -> Jdec.succeed Eq
                "PE" -> Jdec.succeed PE
                somethingElse -> Jdec.fail <| "Invalid InstrumentType: " ++ somethingElse
        )

encodeInstrumentType : InstrumentType -> Jenc.Value
encodeInstrumentType x = case x of
    Ce -> Jenc.string "CE"
    Eq -> Jenc.string "EQ"
    PE -> Jenc.string "PE"

segment : Jdec.Decoder Segment
segment =
    Jdec.string
        |> Jdec.andThen (\str ->
            case str of
                "NFO-OPT" -> Jdec.succeed NfoOpt
                "BSE" -> Jdec.succeed SegmentBSE
                "NSE" -> Jdec.succeed SegmentNSE
                somethingElse -> Jdec.fail <| "Invalid Segment: " ++ somethingElse
        )

encodeSegment : Segment -> Jenc.Value
encodeSegment x = case x of
    NfoOpt -> Jenc.string "NFO-OPT"
    SegmentBSE -> Jenc.string "BSE"
    SegmentNSE -> Jenc.string "NSE"

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

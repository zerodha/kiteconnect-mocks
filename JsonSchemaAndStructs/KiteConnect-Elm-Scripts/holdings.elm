-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import Holdings exposing (holdings)
--
-- and you're off to the races with
--
--     decodeString holdings myJsonString

module Holdings exposing
    ( Holdings
    , holdingsToString
    , holdings
    , Datum
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias Holdings =
    { data : Maybe (Array Datum)
    , status : Maybe String
    }

type alias Datum =
    { authorisedDate : Maybe String
    , authorisedQuantity : Maybe Int
    , averagePrice : Maybe Float
    , closePrice : Maybe Float
    , collateralQuantity : Maybe Int
    , collateralType : Maybe String
    , dayChange : Maybe Float
    , dayChangePercentage : Maybe Float
    , discrepancy : Maybe Bool
    , exchange : Maybe String
    , instrumentToken : Maybe Int
    , isin : Maybe String
    , lastPrice : Maybe Float
    , openingQuantity : Maybe Int
    , pnl : Maybe Float
    , price : Maybe Int
    , product : Maybe String
    , quantity : Maybe Int
    , realisedQuantity : Maybe Int
    , t1Quantity : Maybe Int
    , tradingsymbol : Maybe String
    , usedQuantity : Maybe Int
    }

-- decoders and encoders

holdingsToString : Holdings -> String
holdingsToString r = Jenc.encode 0 (encodeHoldings r)

holdings : Jdec.Decoder Holdings
holdings =
    Jpipe.decode Holdings
        |> Jpipe.optional "data" (Jdec.nullable (Jdec.array datum)) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeHoldings : Holdings -> Jenc.Value
encodeHoldings x =
    Jenc.object
        [ ("data", makeNullableEncoder (makeArrayEncoder encodeDatum) x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
        ]

datum : Jdec.Decoder Datum
datum =
    Jpipe.decode Datum
        |> Jpipe.optional "authorised_date" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "authorised_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "average_price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "close_price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "collateral_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "collateral_type" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "day_change" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "day_change_percentage" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "discrepancy" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "exchange" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "instrument_token" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "isin" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "last_price" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "opening_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "pnl" (Jdec.nullable Jdec.float) Nothing
        |> Jpipe.optional "price" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "product" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "realised_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "t1_quantity" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "tradingsymbol" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "used_quantity" (Jdec.nullable Jdec.int) Nothing

encodeDatum : Datum -> Jenc.Value
encodeDatum x =
    Jenc.object
        [ ("authorised_date", makeNullableEncoder Jenc.string x.authorisedDate)
        , ("authorised_quantity", makeNullableEncoder Jenc.int x.authorisedQuantity)
        , ("average_price", makeNullableEncoder Jenc.float x.averagePrice)
        , ("close_price", makeNullableEncoder Jenc.float x.closePrice)
        , ("collateral_quantity", makeNullableEncoder Jenc.int x.collateralQuantity)
        , ("collateral_type", makeNullableEncoder Jenc.string x.collateralType)
        , ("day_change", makeNullableEncoder Jenc.float x.dayChange)
        , ("day_change_percentage", makeNullableEncoder Jenc.float x.dayChangePercentage)
        , ("discrepancy", makeNullableEncoder Jenc.bool x.discrepancy)
        , ("exchange", makeNullableEncoder Jenc.string x.exchange)
        , ("instrument_token", makeNullableEncoder Jenc.int x.instrumentToken)
        , ("isin", makeNullableEncoder Jenc.string x.isin)
        , ("last_price", makeNullableEncoder Jenc.float x.lastPrice)
        , ("opening_quantity", makeNullableEncoder Jenc.int x.openingQuantity)
        , ("pnl", makeNullableEncoder Jenc.float x.pnl)
        , ("price", makeNullableEncoder Jenc.int x.price)
        , ("product", makeNullableEncoder Jenc.string x.product)
        , ("quantity", makeNullableEncoder Jenc.int x.quantity)
        , ("realised_quantity", makeNullableEncoder Jenc.int x.realisedQuantity)
        , ("t1_quantity", makeNullableEncoder Jenc.int x.t1Quantity)
        , ("tradingsymbol", makeNullableEncoder Jenc.string x.tradingsymbol)
        , ("used_quantity", makeNullableEncoder Jenc.int x.usedQuantity)
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

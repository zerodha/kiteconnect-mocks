-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import GenerateSession exposing (generateSession)
--
-- and you're off to the races with
--
--     decodeString generateSession myJsonString

module GenerateSession exposing
    ( GenerateSession
    , generateSessionToString
    , generateSession
    , Data
    , Meta
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias GenerateSession =
    { data : Maybe Data
    , status : Maybe String
    }

type alias Data =
    { accessToken : Maybe String
    , apiKey : Maybe String
    , avatarURL : Maybe String
    , broker : Maybe String
    , email : Maybe String
    , enctoken : Maybe String
    , exchanges : Maybe (Array String)
    , loginTime : Maybe String
    , meta : Maybe Meta
    , orderTypes : Maybe (Array String)
    , products : Maybe (Array String)
    , publicToken : Maybe String
    , refreshToken : Maybe String
    , silo : Maybe String
    , userID : Maybe String
    , userName : Maybe String
    , userShortname : Maybe String
    , userType : Maybe String
    }

type alias Meta =
    { dematConsent : Maybe String
    }

-- decoders and encoders

generateSessionToString : GenerateSession -> String
generateSessionToString r = Jenc.encode 0 (encodeGenerateSession r)

generateSession : Jdec.Decoder GenerateSession
generateSession =
    Jpipe.decode GenerateSession
        |> Jpipe.optional "data" (Jdec.nullable data) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeGenerateSession : GenerateSession -> Jenc.Value
encodeGenerateSession x =
    Jenc.object
        [ ("data", makeNullableEncoder encodeData x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
        ]

data : Jdec.Decoder Data
data =
    Jpipe.decode Data
        |> Jpipe.optional "access_token" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "api_key" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "avatar_url" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "broker" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "email" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "enctoken" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "exchanges" (Jdec.nullable (Jdec.array Jdec.string)) Nothing
        |> Jpipe.optional "login_time" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "meta" (Jdec.nullable meta) Nothing
        |> Jpipe.optional "order_types" (Jdec.nullable (Jdec.array Jdec.string)) Nothing
        |> Jpipe.optional "products" (Jdec.nullable (Jdec.array Jdec.string)) Nothing
        |> Jpipe.optional "public_token" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "refresh_token" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "silo" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "user_id" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "user_name" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "user_shortname" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "user_type" (Jdec.nullable Jdec.string) Nothing

encodeData : Data -> Jenc.Value
encodeData x =
    Jenc.object
        [ ("access_token", makeNullableEncoder Jenc.string x.accessToken)
        , ("api_key", makeNullableEncoder Jenc.string x.apiKey)
        , ("avatar_url", makeNullableEncoder Jenc.string x.avatarURL)
        , ("broker", makeNullableEncoder Jenc.string x.broker)
        , ("email", makeNullableEncoder Jenc.string x.email)
        , ("enctoken", makeNullableEncoder Jenc.string x.enctoken)
        , ("exchanges", makeNullableEncoder (makeArrayEncoder Jenc.string) x.exchanges)
        , ("login_time", makeNullableEncoder Jenc.string x.loginTime)
        , ("meta", makeNullableEncoder encodeMeta x.meta)
        , ("order_types", makeNullableEncoder (makeArrayEncoder Jenc.string) x.orderTypes)
        , ("products", makeNullableEncoder (makeArrayEncoder Jenc.string) x.products)
        , ("public_token", makeNullableEncoder Jenc.string x.publicToken)
        , ("refresh_token", makeNullableEncoder Jenc.string x.refreshToken)
        , ("silo", makeNullableEncoder Jenc.string x.silo)
        , ("user_id", makeNullableEncoder Jenc.string x.userID)
        , ("user_name", makeNullableEncoder Jenc.string x.userName)
        , ("user_shortname", makeNullableEncoder Jenc.string x.userShortname)
        , ("user_type", makeNullableEncoder Jenc.string x.userType)
        ]

meta : Jdec.Decoder Meta
meta =
    Jpipe.decode Meta
        |> Jpipe.optional "demat_consent" (Jdec.nullable Jdec.string) Nothing

encodeMeta : Meta -> Jenc.Value
encodeMeta x =
    Jenc.object
        [ ("demat_consent", makeNullableEncoder Jenc.string x.dematConsent)
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

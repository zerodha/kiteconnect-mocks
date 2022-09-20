-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)`);
--     import Profile exposing (profile)
--
-- and you're off to the races with
--
--     decodeString profile myJsonString

module Profile exposing
    ( Profile
    , profileToString
    , profile
    , Data
    , Meta
    )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)

type alias Profile =
    { data : Maybe Data
    , status : Maybe String
    }

type alias Data =
    { avatarURL : Maybe ()
    , broker : Maybe String
    , email : Maybe String
    , exchanges : Maybe (Array String)
    , meta : Maybe Meta
    , orderTypes : Maybe (Array String)
    , products : Maybe (Array String)
    , userID : Maybe String
    , userName : Maybe String
    , userShortname : Maybe String
    , userType : Maybe String
    }

type alias Meta =
    { dematConsent : Maybe String
    }

-- decoders and encoders

profileToString : Profile -> String
profileToString r = Jenc.encode 0 (encodeProfile r)

profile : Jdec.Decoder Profile
profile =
    Jpipe.decode Profile
        |> Jpipe.optional "data" (Jdec.nullable data) Nothing
        |> Jpipe.optional "status" (Jdec.nullable Jdec.string) Nothing

encodeProfile : Profile -> Jenc.Value
encodeProfile x =
    Jenc.object
        [ ("data", makeNullableEncoder encodeData x.data)
        , ("status", makeNullableEncoder Jenc.string x.status)
        ]

data : Jdec.Decoder Data
data =
    Jpipe.decode Data
        |> Jpipe.optional "avatar_url" (Jdec.nullable (Jdec.null ())) Nothing
        |> Jpipe.optional "broker" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "email" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "exchanges" (Jdec.nullable (Jdec.array Jdec.string)) Nothing
        |> Jpipe.optional "meta" (Jdec.nullable meta) Nothing
        |> Jpipe.optional "order_types" (Jdec.nullable (Jdec.array Jdec.string)) Nothing
        |> Jpipe.optional "products" (Jdec.nullable (Jdec.array Jdec.string)) Nothing
        |> Jpipe.optional "user_id" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "user_name" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "user_shortname" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "user_type" (Jdec.nullable Jdec.string) Nothing

encodeData : Data -> Jenc.Value
encodeData x =
    Jenc.object
        [ ("avatar_url", makeNullableEncoder (always Jenc.null) x.avatarURL)
        , ("broker", makeNullableEncoder Jenc.string x.broker)
        , ("email", makeNullableEncoder Jenc.string x.email)
        , ("exchanges", makeNullableEncoder (makeArrayEncoder Jenc.string) x.exchanges)
        , ("meta", makeNullableEncoder encodeMeta x.meta)
        , ("order_types", makeNullableEncoder (makeArrayEncoder Jenc.string) x.orderTypes)
        , ("products", makeNullableEncoder (makeArrayEncoder Jenc.string) x.products)
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

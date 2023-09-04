module Config.UserSettings exposing (..)

import Config.Graph exposing (AddressLabelType(..), TxLabelType(..))
import Json.Decode as Decode exposing (Decoder, bool, decodeString, float, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode
import Model.Currency exposing (Currency(..))
import Model.Locale exposing (ValueDetail(..))


type alias UserSettings =
    { selectedLanguage : String
    , lightMode : Maybe Bool
    , valueDetail : Maybe Model.Locale.ValueDetail
    , valueDenomination : Maybe Currency
    , addressLabel : Maybe AddressLabelType
    , edgeLabel : Maybe TxLabelType
    , showAddressShadowLinks : Maybe Bool
    , showClusterShadowLinks : Maybe Bool
    , showDatesInUserLocale : Maybe Bool
    }


stringToValueDetail : String -> ValueDetail
stringToValueDetail s =
    case s of
        "exact" ->
            Exact

        "magnitude" ->
            Magnitude

        _ ->
            Magnitude


valueDetailToString : ValueDetail -> String
valueDetailToString d =
    case d of
        Exact ->
            "exact"

        Magnitude ->
            "magnitude"


currencyToString : Currency -> String
currencyToString c =
    case c of
        Coin ->
            "coin"

        Fiat x ->
            x


stringToCurrency : String -> Currency
stringToCurrency s =
    case s of
        "coin" ->
            Coin

        x ->
            Fiat x


addressLabelToString : AddressLabelType -> String
addressLabelToString c =
    case c of
        ID ->
            "id"

        Balance ->
            "balance"

        Tag ->
            "tag"


stringToAddressLabel : String -> AddressLabelType
stringToAddressLabel s =
    case s of
        "id" ->
            ID

        "balance" ->
            Balance

        "tag" ->
            Tag

        _ ->
            ID


edgeLabelToString : TxLabelType -> String
edgeLabelToString c =
    case c of
        NoTxs ->
            "notxs"

        Value ->
            "value"


stringToEdgeLabel : String -> TxLabelType
stringToEdgeLabel s =
    case s of
        "notxs" ->
            NoTxs

        "value" ->
            Value

        _ ->
            Value


decoder : Decoder UserSettings
decoder =
    Decode.succeed UserSettings
        |> required "selectedLanguage" string
        |> optional "lightMode" (nullable bool) Nothing
        |> optional "valueDetail" (nullable (Decode.string |> Decode.map stringToValueDetail)) Nothing
        |> optional "valueDenomination" (nullable (Decode.string |> Decode.map stringToCurrency)) Nothing
        |> optional "addressLabel" (nullable (Decode.string |> Decode.map stringToAddressLabel)) Nothing
        |> optional "edgeLabel" (nullable (Decode.string |> Decode.map stringToEdgeLabel)) Nothing
        |> optional "showAddressShadowLinks" (nullable bool) Nothing
        |> optional "showClusterShadowLinks" (nullable bool) Nothing
        |> optional "showDatesInUserLocale" (nullable bool) Nothing


encoder : UserSettings -> Json.Encode.Value
encoder settings =
    Json.Encode.object
        [ ( "selectedLanguage", Json.Encode.string settings.selectedLanguage )
        , ( "lightMode", settings.lightMode |> Maybe.map Json.Encode.bool |> Maybe.withDefault Json.Encode.null )
        , ( "valueDetail", settings.valueDetail |> Maybe.map valueDetailToString |> Maybe.map Json.Encode.string |> Maybe.withDefault Json.Encode.null )
        , ( "valueDenomination", settings.valueDenomination |> Maybe.map currencyToString |> Maybe.map Json.Encode.string |> Maybe.withDefault Json.Encode.null )
        , ( "addressLabel", settings.addressLabel |> Maybe.map addressLabelToString |> Maybe.map Json.Encode.string |> Maybe.withDefault Json.Encode.null )
        , ( "edgeLabel", settings.edgeLabel |> Maybe.map edgeLabelToString |> Maybe.map Json.Encode.string |> Maybe.withDefault Json.Encode.null )
        , ( "showAddressShadowLinks", settings.showAddressShadowLinks |> Maybe.map Json.Encode.bool |> Maybe.withDefault Json.Encode.null )
        , ( "showClusterShadowLinks", settings.showClusterShadowLinks |> Maybe.map Json.Encode.bool |> Maybe.withDefault Json.Encode.null )
        , ( "showDatesInUserLocale", settings.showDatesInUserLocale |> Maybe.map Json.Encode.bool |> Maybe.withDefault Json.Encode.null )
        ]


default : UserSettings
default =
    { selectedLanguage = "en"
    , lightMode = Nothing
    , valueDetail = Nothing
    , valueDenomination = Nothing
    , addressLabel = Nothing
    , edgeLabel = Nothing
    , showAddressShadowLinks = Nothing
    , showClusterShadowLinks = Nothing
    , showDatesInUserLocale = Nothing
    }

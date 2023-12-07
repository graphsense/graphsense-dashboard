module Config.UserSettings exposing (..)

import Config.Graph exposing (AddressLabelType(..), TxLabelType(..))
import Json.Decode as Decode exposing (Decoder, bool, nullable, string)
import Json.Decode.Extra
import Json.Decode.Pipeline exposing (optional, required)
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
    , showZeroValueTxs : Maybe Bool
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


fromString : Decoder a -> Decoder a
fromString dec =
    Decode.string
        |> Decode.andThen
            (Decode.decodeString dec
                >> Result.mapError Decode.errorToString
                >> Json.Decode.Extra.fromResult
            )


decoder : Decoder UserSettings
decoder =
    Decode.succeed UserSettings
        |> required "selectedLanguage" string
        |> optional "lightMode" (nullable bool |> fromString) Nothing
        |> optional "valueDetail" (Decode.string |> Decode.map stringToValueDetail |> nullable) Nothing
        |> optional "valueDenomination" (Decode.string |> Decode.map stringToCurrency |> nullable) Nothing
        |> optional "addressLabel" (Decode.string |> Decode.map stringToAddressLabel |> nullable) Nothing
        |> optional "edgeLabel" (Decode.string |> Decode.map stringToEdgeLabel |> nullable) Nothing
        |> optional "showAddressShadowLinks" (nullable bool |> fromString) Nothing
        |> optional "showClusterShadowLinks" (nullable bool |> fromString) Nothing
        |> optional "showDatesInUserLocale" (nullable bool |> fromString) Nothing
        |> optional "showZeroValueTxs" (nullable bool |> fromString) Nothing


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
        , ( "showZeroValueTxs", settings.showZeroValueTxs |> Maybe.map Json.Encode.bool |> Maybe.withDefault Json.Encode.null )
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
    , showZeroValueTxs = Nothing
    }

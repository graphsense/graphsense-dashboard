module Config.UserSettings exposing (UserSettings, decoder, default, encoder)

-- import Model.Currency exposing (Currency(..))

import Config.Graph exposing (AddressLabelType, TxLabelType(..), addressLabelToString, stringToAddressLabel)
import Config.Pathfinder exposing (TracingMode(..))
import Json.Decode as Decode exposing (Decoder, bool, nullable, string)
import Json.Decode.Extra
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode
import Model.Locale exposing (ValueDetail(..))


type alias UserSettings =
    { selectedLanguage : String
    , lightMode : Maybe Bool
    , valueDetail : Maybe Model.Locale.ValueDetail
    , preferredFiatCurrency : Maybe String
    , showValuesInFiat : Maybe Bool
    , addressLabel : Maybe AddressLabelType
    , edgeLabel : Maybe TxLabelType
    , showAddressShadowLinks : Maybe Bool
    , showClusterShadowLinks : Maybe Bool
    , showDatesInUserLocale : Maybe Bool
    , showZeroValueTxs : Maybe Bool
    , showTimeZoneOffset : Maybe Bool
    , highlightClusterFriends : Maybe Bool
    , showTimestampOnTxEdge : Maybe Bool
    , snapToGrid : Maybe Bool
    , tracingMode : Maybe TracingMode
    , showHash : Maybe Bool
    , showBothValues : Maybe Bool
    , avoidOverlapingNodes : Maybe Bool

    -- , showLabelsInTaggingOverview : Maybe Bool
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
        |> optional "preferredFiatCurrency" (Decode.string |> nullable) Nothing
        |> optional "showValuesInFiat" (nullable bool |> fromString) Nothing
        |> optional "addressLabel" (Decode.string |> Decode.map stringToAddressLabel) Nothing
        |> optional "edgeLabel" (Decode.string |> Decode.map stringToEdgeLabel |> nullable) Nothing
        |> optional "showAddressShadowLinks" (nullable bool |> fromString) Nothing
        |> optional "showClusterShadowLinks" (nullable bool |> fromString) Nothing
        |> optional "showDatesInUserLocale" (nullable bool |> fromString) Nothing
        |> optional "showZeroValueTxs" (nullable bool |> fromString) Nothing
        |> optional "showTimeZoneOffset" (nullable bool |> fromString) Nothing
        |> optional "highlightClusterFriends" (nullable bool |> fromString) Nothing
        |> optional "showTimestampOnTxEdge" (nullable bool |> fromString) Nothing
        |> optional "snapToGrid" (nullable bool |> fromString) Nothing
        |> optional "tracingMode" (Decode.string |> Decode.map stringToTracingMode |> nullable) Nothing
        |> optional "showHash" (nullable bool |> fromString) Nothing
        |> optional "showBothValues" (nullable bool |> fromString) Nothing
        |> optional "avoidOverlapingNodes" (nullable bool |> fromString) Nothing


stringToTracingMode : String -> TracingMode
stringToTracingMode arg1 =
    case arg1 of
        "transaction" ->
            TransactionTracingMode

        "aggregate" ->
            AggregateTracingMode

        _ ->
            TransactionTracingMode



-- |> optional "showLabelsInTaggingOverview" (nullable bool |> fromString) Nothing


encoder : UserSettings -> Json.Encode.Value
encoder settings =
    Json.Encode.object
        [ ( "selectedLanguage", Json.Encode.string settings.selectedLanguage )
        , ( "lightMode", settings.lightMode |> Maybe.map Json.Encode.bool |> Maybe.withDefault Json.Encode.null )
        , ( "valueDetail", settings.valueDetail |> Maybe.map valueDetailToString |> Maybe.map Json.Encode.string |> Maybe.withDefault Json.Encode.null )
        , ( "preferredFiatCurrency", settings.preferredFiatCurrency |> Maybe.map Json.Encode.string |> Maybe.withDefault Json.Encode.null )
        , ( "showValuesInFiat", settings.showValuesInFiat |> Maybe.map Json.Encode.bool |> Maybe.withDefault Json.Encode.null )
        , ( "addressLabel", settings.addressLabel |> Maybe.map addressLabelToString |> Maybe.map Json.Encode.string |> Maybe.withDefault Json.Encode.null )
        , ( "edgeLabel", settings.edgeLabel |> Maybe.map edgeLabelToString |> Maybe.map Json.Encode.string |> Maybe.withDefault Json.Encode.null )
        , ( "showAddressShadowLinks", settings.showAddressShadowLinks |> Maybe.map Json.Encode.bool |> Maybe.withDefault Json.Encode.null )
        , ( "showClusterShadowLinks", settings.showClusterShadowLinks |> Maybe.map Json.Encode.bool |> Maybe.withDefault Json.Encode.null )
        , ( "showDatesInUserLocale", settings.showDatesInUserLocale |> Maybe.map Json.Encode.bool |> Maybe.withDefault Json.Encode.null )
        , ( "showZeroValueTxs", settings.showZeroValueTxs |> Maybe.map Json.Encode.bool |> Maybe.withDefault Json.Encode.null )
        , ( "showTimeZoneOffset", settings.showTimeZoneOffset |> Maybe.map Json.Encode.bool |> Maybe.withDefault Json.Encode.null )
        , ( "highlightClusterFriends", settings.highlightClusterFriends |> Maybe.map Json.Encode.bool |> Maybe.withDefault Json.Encode.null )
        , ( "showTimestampOnTxEdge", settings.showTimestampOnTxEdge |> Maybe.map Json.Encode.bool |> Maybe.withDefault Json.Encode.null )
        , ( "snapToGrid", settings.snapToGrid |> Maybe.map Json.Encode.bool |> Maybe.withDefault Json.Encode.null )
        , ( "tracingMode", settings.tracingMode |> Maybe.map tracingModeToString |> Maybe.map Json.Encode.string |> Maybe.withDefault Json.Encode.null )
        , ( "showHash", settings.showHash |> Maybe.map Json.Encode.bool |> Maybe.withDefault Json.Encode.null )
        , ( "showBothValues", settings.showBothValues |> Maybe.map Json.Encode.bool |> Maybe.withDefault Json.Encode.null )
        , ( "avoidOverlapingNodes", settings.avoidOverlapingNodes |> Maybe.map Json.Encode.bool |> Maybe.withDefault Json.Encode.null )

        -- , ( "showLabelsInTaggingOverview", settings.showLabelsInTaggingOverview |> Maybe.map Json.Encode.bool |> Maybe.withDefault Json.Encode.null )
        ]


tracingModeToString : TracingMode -> String
tracingModeToString arg1 =
    case arg1 of
        TransactionTracingMode ->
            "transaction"

        AggregateTracingMode ->
            "aggregate"


default : UserSettings
default =
    { selectedLanguage = "en"
    , lightMode = Nothing
    , valueDetail = Nothing
    , preferredFiatCurrency = Nothing
    , showValuesInFiat = Nothing
    , addressLabel = Nothing
    , edgeLabel = Nothing
    , showAddressShadowLinks = Nothing
    , showClusterShadowLinks = Nothing
    , showDatesInUserLocale = Nothing
    , showZeroValueTxs = Nothing
    , showTimeZoneOffset = Nothing
    , highlightClusterFriends = Nothing
    , showTimestampOnTxEdge = Nothing
    , snapToGrid = Nothing
    , tracingMode = Nothing
    , showHash = Nothing
    , showBothValues = Nothing
    , avoidOverlapingNodes = Nothing

    -- , showLabelsInTaggingOverview = Nothing
    }

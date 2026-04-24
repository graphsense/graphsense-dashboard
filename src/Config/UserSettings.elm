module Config.UserSettings exposing (UserSettings, decoder, default, encoder)

-- import Model.Currency exposing (Currency(..))

import Config.Graph exposing (AddressLabelType, TxLabelType(..), addressLabelToString, stringToAddressLabel)
import Config.Pathfinder exposing (TracingMode(..))
import Json.Decode as Decode exposing (Decoder, bool, int, nullable, string)
import Json.Decode.Extra
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode
import Model.Locale exposing (ValueDetail(..))
import Model.Search exposing (ResultLine(..), persistRecentSearches)


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
    , recentSearches : List ResultLine

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
        |> optional "recentSearches"
            (Decode.oneOf
                [ if persistRecentSearches then
                    Decode.list resultLineDecoder |> fromString

                  else
                    Decode.succeed []
                , Decode.succeed []
                ]
            )
            []


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
        , ( "recentSearches"
          , (if persistRecentSearches then
                settings.recentSearches

             else
                []
            )
                |> Json.Encode.list encodeResultLine
                |> Json.Encode.encode 0
                |> Json.Encode.string
          )

        -- , ( "showLabelsInTaggingOverview", settings.showLabelsInTaggingOverview |> Maybe.map Json.Encode.bool |> Maybe.withDefault Json.Encode.null )
        ]


encodeResultLine : ResultLine -> Json.Encode.Value
encodeResultLine rl =
    case rl of
        Address currency addr ->
            Json.Encode.object
                [ ( "kind", Json.Encode.string "address" )
                , ( "currency", Json.Encode.string currency )
                , ( "id", Json.Encode.string addr )
                ]

        Tx currency tx ->
            Json.Encode.object
                [ ( "kind", Json.Encode.string "tx" )
                , ( "currency", Json.Encode.string currency )
                , ( "id", Json.Encode.string tx )
                ]

        Block currency height ->
            Json.Encode.object
                [ ( "kind", Json.Encode.string "block" )
                , ( "currency", Json.Encode.string currency )
                , ( "height", Json.Encode.int height )
                ]

        Label label ->
            Json.Encode.object
                [ ( "kind", Json.Encode.string "label" )
                , ( "label", Json.Encode.string label )
                ]

        Actor ( id, label ) ->
            Json.Encode.object
                [ ( "kind", Json.Encode.string "actor" )
                , ( "id", Json.Encode.string id )
                , ( "label", Json.Encode.string label )
                ]

        Custom { id, label } ->
            Json.Encode.object
                [ ( "kind", Json.Encode.string "custom" )
                , ( "id", Json.Encode.string id )
                , ( "label", Json.Encode.string label )
                ]


resultLineDecoder : Decoder ResultLine
resultLineDecoder =
    Decode.field "kind" string
        |> Decode.andThen
            (\kind ->
                case kind of
                    "address" ->
                        Decode.map2 Address
                            (Decode.field "currency" string)
                            (Decode.field "id" string)

                    "tx" ->
                        Decode.map2 Tx
                            (Decode.field "currency" string)
                            (Decode.field "id" string)

                    "block" ->
                        Decode.map2 Block
                            (Decode.field "currency" string)
                            (Decode.field "height" int)

                    "label" ->
                        Decode.map Label
                            (Decode.field "label" string)

                    "actor" ->
                        Decode.map2 (\id label -> Actor ( id, label ))
                            (Decode.field "id" string)
                            (Decode.field "label" string)

                    "custom" ->
                        Decode.map2 (\id label -> Custom { id = id, label = label })
                            (Decode.field "id" string)
                            (Decode.field "label" string)

                    _ ->
                        Decode.fail ("Unknown recent search kind: " ++ kind)
            )


tracingModeToString : TracingMode -> String
tracingModeToString arg1 =
    case arg1 of
        TransactionTracingMode ->
            "transaction"

        AggregateTracingMode ->
            "aggregate"


default : String -> UserSettings
default locale =
    { selectedLanguage = locale
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
    , recentSearches = []

    -- , showLabelsInTaggingOverview = Nothing
    }

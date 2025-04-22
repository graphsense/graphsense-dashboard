module View.Pathfinder.Tooltip exposing (linkRow, tooltipRow, tooltipRowCustomValue, view)

import Api.Data exposing (Actor, TagSummary)
import Config.View as View exposing (getConceptName)
import Css
import Css.Pathfinder as Css
import Dict
import Html.Styled exposing (Html, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href, target, title)
import Html.Styled.Events exposing (onMouseEnter, onMouseLeave)
import Model exposing (Msg)
import Model.Currency exposing (assetFromBase)
import Model.Pathfinder.Address as Addr
import Model.Pathfinder.Id as Id
import Model.Pathfinder.Tooltip exposing (Tooltip, TooltipType(..))
import Plugin.Model
import Plugin.View as Plugin exposing (Plugins)
import RecordSetter as Rs
import Set
import Theme.Html.GraphComponents as GraphComponents
import Theme.Html.TagsComponents as TagsComponents
import Tuple exposing (pair)
import Util.Css as Css
import Util.Pathfinder.TagConfidence exposing (ConfidenceRange(..), getConfidenceRangeFromFloat)
import Util.Pathfinder.TagSummary as TagSummary
import Util.View exposing (hovercardFullViewPort, none, truncateLongIdentifierWithLengths)
import View.Button as Button
import View.Locale as Locale


view : Plugins -> Plugin.Model.ModelState -> View.Config -> Tooltip Msg -> Html Msg
view plugins pluginStates vc tt =
    let
        ( content, containerAttributes ) =
            case tt.type_ of
                UtxoTx t ->
                    ( genericTx vc { txId = t.raw.txHash, timestamp = t.raw.timestamp }, [] )

                AccountTx t ->
                    ( genericTx vc { txId = t.raw.identifier, timestamp = t.raw.timestamp }, [] )

                Address a ts ->
                    ( address vc ts a, [] )

                TagLabel lblid x msgs ->
                    ( tagLabel vc lblid x, [ onMouseEnter msgs.openTooltip, onMouseLeave msgs.closeTooltip ] )

                TagConcept _ conceptId x msgs ->
                    ( tagConcept vc msgs.openDetails conceptId x, [ onMouseEnter msgs.openTooltip, onMouseLeave msgs.closeTooltip ] )

                ActorDetails ac msgs ->
                    ( showActor vc ac, [ onMouseEnter msgs.openTooltip, onMouseLeave msgs.closeTooltip ] )

                Text t ->
                    ( [ div [ [ Css.width (Css.px GraphComponents.tooltipDown_details.width) ] |> css ] [ text t ] ], [] )

                Plugin s msgs ->
                    ( Plugin.tooltip plugins s pluginStates vc |> Maybe.withDefault [], [ onMouseEnter msgs.openTooltip, onMouseLeave msgs.closeTooltip ] )
    in
    content
        |> div
            (css (GraphComponents.tooltipDown_details.styles ++ [ Css.minWidth (Css.px 230) ])
                :: containerAttributes
            )
        |> toUnstyled
        |> List.singleton
        |> hovercardFullViewPort vc tt.hovercard (Css.zIndexMainValue + 10000)


getConfidenceIndicator : View.Config -> Float -> Html msg
getConfidenceIndicator vc x =
    let
        r =
            getConfidenceRangeFromFloat x

        lbl =
            case r of
                High ->
                    "High confidence"

                Medium ->
                    "Medium confidence"

                Low ->
                    "Low confidence"

        cl =
            case r of
                High ->
                    TagsComponents.ConfidenceLevelConfidenceLevelHigh

                Medium ->
                    TagsComponents.ConfidenceLevelConfidenceLevelMedium

                Low ->
                    TagsComponents.ConfidenceLevelConfidenceLevelLow
    in
    TagsComponents.confidenceLevel
        { root =
            { size = TagsComponents.ConfidenceLevelSizeSmall
            , confidenceLevel = cl
            , text = Locale.string vc.locale lbl
            }
        }


val : View.Config -> String -> { firstRowText : String, secondRowText : String, secondRowVisible : Bool }
val vc str =
    { firstRowText = Locale.string vc.locale str
    , secondRowText = ""
    , secondRowVisible = False
    }


baseRowStyle : List Css.Style
baseRowStyle =
    [ Css.width (Css.pct 100), Css.fontSize (Css.px 14) ]


tooltipRow : { tooltipRowLabel : { title : String }, tooltipRowValue : { firstRowText : String, secondRowVisible : Bool, secondRowText : String } } -> Html msg
tooltipRow =
    GraphComponents.tooltipRowWithAttributes
        (GraphComponents.tooltipRowAttributes
            |> Rs.s_root [ css baseRowStyle ]
        )


tooltipRowCustomValue : String -> Html msg -> Html msg
tooltipRowCustomValue title rowValue =
    GraphComponents.tooltipRowWithInstances
        (GraphComponents.tooltipRowAttributes
            |> Rs.s_root [ css baseRowStyle ]
        )
        (GraphComponents.tooltipRowInstances |> Rs.s_tooltipRowValue (Just rowValue))
        { tooltipRowLabel = { title = title }
        , tooltipRowValue =
            { firstRowText = "", secondRowText = "", secondRowVisible = False }
        }


linkRow : View.Config -> String -> msg -> Html msg
linkRow vc txt msg =
    tooltipRowCustomValue ""
        (Button.defaultConfig
            |> Rs.s_text txt
            |> Rs.s_onClick (Just msg)
            |> Button.linkButtonBlue vc
        )


showActor : View.Config -> Actor -> List (Html msg)
showActor vc a =
    let
        mainUri =
            if not (String.startsWith "http://" a.uri) && not (String.startsWith "https://" a.uri) then
                "https://" ++ a.uri

            else
                a.uri
    in
    [ tooltipRow
        { tooltipRowLabel = { title = Locale.string vc.locale "Actor" }
        , tooltipRowValue = a.label |> val vc
        }
    , tooltipRowCustomValue (Locale.string vc.locale "Url") (Html.Styled.a [ Css.plainLinkStyle vc |> css, href mainUri, target "blank" ] [ text a.uri ])
    , tooltipRowCustomValue
        (Locale.string vc.locale "Jurisdictions")
        (let
            jl =
                List.length a.jurisdictions
         in
         a.jurisdictions
            |> List.indexedMap
                (\i z ->
                    div
                        [ title (Locale.string vc.locale z.label)
                        , Css.mGap
                            |> Css.paddingRight
                            |> List.singleton
                            |> css
                        ]
                        [ text
                            (Locale.string vc.locale z.label
                                ++ (if i /= (jl - 1) then
                                        ", "

                                    else
                                        ""
                                   )
                            )
                        ]
                )
            |> div [ [ Css.displayFlex, Css.flexDirection Css.column ] |> css ]
        )
    ]


tagConcept : View.Config -> Maybe msg -> String -> TagSummary -> List (Html msg)
tagConcept vc openDetailsMsg concept tag =
    let
        relevantLabels =
            Dict.toList tag.labelSummary |> List.filter (Tuple.second >> (.concepts >> List.member concept)) |> List.map Tuple.second

        labels =
            relevantLabels |> List.map .label

        maxConfidence =
            relevantLabels |> List.map .confidence |> List.maximum |> Maybe.withDefault 0

        -- tagCount =
        --     relevantLabels |> List.map .count |> List.sum
        sources =
            relevantLabels |> List.map .sources |> List.foldr (++) [] |> Set.fromList
    in
    [ tooltipRowCustomValue
        (Locale.string
            vc.locale
            "Labels"
        )
        (let
            jl =
                List.length labels

            max_labels =
                7
         in
         labels
            |> List.indexedMap
                (\i z ->
                    if i <= max_labels then
                        div
                            [ Css.mGap
                                |> Css.paddingRight
                                |> List.singleton
                                |> css
                            ]
                            [ text
                                (z
                                    ++ (if i /= (jl - 1) then
                                            ", "

                                        else
                                            ""
                                       )
                                )
                            ]

                    else if i == (max_labels + 1) then
                        div
                            [ title (String.join ", " labels)
                            , Css.mGap
                                |> Css.paddingRight
                                |> List.singleton
                                |> css
                            ]
                            [ text "..."
                            ]

                    else
                        none
                )
            |> div [ title (String.join ", " labels), [ Css.displayFlex, Css.flexDirection Css.column ] |> css ]
        )
    , tooltipRowCustomValue (Locale.string vc.locale "Confidence") (getConfidenceIndicator vc maxConfidence)
    , tooltipRow
        { tooltipRowLabel = { title = Locale.string vc.locale "Sources" }
        , tooltipRowValue =
            Set.size sources
                |> String.fromInt
                |> val vc
        }
    , openDetailsMsg |> Maybe.map (linkRow vc "Learn more") |> Maybe.withDefault none
    ]


tagLabel : View.Config -> String -> TagSummary -> List (Html msg)
tagLabel vc lbl tag =
    let
        mlbldata =
            Dict.get lbl tag.labelSummary
    in
    case mlbldata of
        Just lbldata ->
            [ tooltipRow
                { tooltipRowLabel = { title = Locale.string vc.locale "Tag label" }
                , tooltipRowValue = lbldata.label |> val vc
                }
            , tooltipRowCustomValue (Locale.string vc.locale "Confidence") (getConfidenceIndicator vc lbldata.confidence)
            ]
                ++ (if List.isEmpty lbldata.concepts then
                        []

                    else
                        tooltipRow
                            { tooltipRowLabel = { title = Locale.string vc.locale "Categories" }
                            , tooltipRowValue =
                                lbldata.concepts
                                    |> List.map (\x -> getConceptName vc x |> Maybe.withDefault x)
                                    |> String.join ", "
                                    |> Locale.string vc.locale
                                    |> Util.View.truncate 20
                                    |> val vc
                            }
                            |> List.singleton
                   )
                ++ [ tooltipRow
                        { tooltipRowLabel = { title = Locale.string vc.locale "Sources" }
                        , tooltipRowValue =
                            List.length lbldata.sources
                                |> String.fromInt
                                |> val vc
                        }
                   , tooltipRow
                        { tooltipRowLabel = { title = Locale.string vc.locale "Mentions" }
                        , tooltipRowValue = lbldata.count |> String.fromInt |> val vc
                        }
                   , tooltipRow
                        { tooltipRowLabel = { title = Locale.string vc.locale "Last modified" }
                        , tooltipRowValue =
                            let
                                date =
                                    Locale.timestampDateUniform vc.locale lbldata.lastmod

                                time =
                                    Locale.timestampTimeUniform vc.locale vc.showTimeZoneOffset lbldata.lastmod
                            in
                            { firstRowText = date
                            , secondRowText = time
                            , secondRowVisible = True
                            }
                        }
                   ]

        _ ->
            []


address : View.Config -> Maybe TagSummary -> Addr.Address -> List (Html msg)
address vc tags adr =
    let
        net =
            Id.network adr.id
    in
    [ tooltipRow
        { tooltipRowLabel = { title = Locale.string vc.locale "Balance" }
        , tooltipRowValue =
            Addr.getBalance adr
                |> Maybe.map
                    (pair (assetFromBase net)
                        >> List.singleton
                        >> Locale.currency vc.locale
                    )
                |> Maybe.withDefault ""
                |> val vc
        }
    , tooltipRow
        { tooltipRowLabel = { title = Locale.string vc.locale "Total received" }
        , tooltipRowValue =
            Addr.getTotalReceived adr
                |> Maybe.map
                    (pair (assetFromBase net)
                        >> List.singleton
                        >> Locale.currency vc.locale
                    )
                |> Maybe.withDefault ""
                |> val vc
        }
    , tooltipRow
        { tooltipRowLabel = { title = Locale.string vc.locale "Total sent" }
        , tooltipRowValue =
            Addr.getTotalSpent adr
                |> Maybe.map
                    (pair (assetFromBase net)
                        >> List.singleton
                        >> Locale.currency vc.locale
                    )
                |> Maybe.withDefault ""
                |> val vc
        }
    ]
        ++ (case tags of
                Just ts ->
                    [ tooltipRow
                        { tooltipRowLabel = { title = Locale.string vc.locale "Tags" }
                        , tooltipRowValue =
                            ts
                                |> TagSummary.getLabelPreview 20
                                |> val vc
                        }
                    ]

                _ ->
                    []
           )


genericTx : View.Config -> { txId : String, timestamp : Int } -> List (Html msg)
genericTx vc tx =
    [ tooltipRow
        { tooltipRowLabel = { title = Locale.string vc.locale "Tx hash" }
        , tooltipRowValue =
            tx.txId
                |> truncateLongIdentifierWithLengths 8 4
                |> val vc
        }
    , tooltipRow
        { tooltipRowLabel = { title = Locale.string vc.locale "Timestamp" }
        , tooltipRowValue =
            let
                date =
                    Locale.timestampDateUniform vc.locale tx.timestamp

                time =
                    Locale.timestampTimeUniform vc.locale vc.showTimeZoneOffset tx.timestamp
            in
            { firstRowText = date
            , secondRowText = time
            , secondRowVisible = True
            }
        }
    ]

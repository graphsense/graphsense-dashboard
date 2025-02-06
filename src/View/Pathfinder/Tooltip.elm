module View.Pathfinder.Tooltip exposing (view)

import Api.Data exposing (Actor, TagSummary)
import Config.View as View exposing (getConceptName)
import Css
import Css.Pathfinder as Css
import Dict
import Html.Styled exposing (Html, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href, target, title)
import Html.Styled.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Model exposing (Msg)
import Model.Currency exposing (assetFromBase)
import Model.Pathfinder.Address as Addr
import Model.Pathfinder.Id as Id
import Model.Pathfinder.Tooltip exposing (Tooltip, TooltipType(..))
import Plugin.Model
import Plugin.View as Plugin exposing (Plugins)
import RecordSetter as Rs
import Set
import Theme.Html.Buttons as Buttons
import Theme.Html.GraphComponents as GraphComponents
import Theme.Html.TagsComponents as TagComponents
import Tuple exposing (pair)
import Util.Css as Css
import Util.Pathfinder.TagConfidence exposing (ConfidenceRange(..), getConfidenceRangeFromFloat)
import Util.Pathfinder.TagSummary as TagSummary
import Util.View exposing (hovercardFullViewPort, none, truncateLongIdentifierWithLengths)
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

                TagConcept aid conceptId x msgs ->
                    ( tagConcept vc msgs.openDetails conceptId x, [ onMouseEnter msgs.openTooltip, onMouseLeave msgs.closeTooltip ] )

                ActorDetails ac msgs ->
                    ( showActor vc ac, [ onMouseEnter msgs.openTooltip, onMouseLeave msgs.closeTooltip ] )

                Text t ->
                    ( [ div [ [ Css.width (Css.px GraphComponents.tooltipProperty1Down_details.width) ] |> css ] [ text t ] ], [] )

                Plugin s ->
                    ( Plugin.tooltip plugins s pluginStates vc |> Maybe.withDefault [], [] )
    in
    content
        |> div
            (css (GraphComponents.tooltipProperty1Down_details.styles ++ [ Css.minWidth (Css.px 200) ])
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
    in
    case r of
        High ->
            TagComponents.confidenceLevelConfidenceLevelHighSizeSmall { confidenceLevelHighSizeSmall = { text = Locale.string vc.locale "High" } }

        Medium ->
            TagComponents.confidenceLevelConfidenceLevelMediumSizeSmall { confidenceLevelMediumSizeSmall = { text = Locale.string vc.locale "Medium" } }

        Low ->
            TagComponents.confidenceLevelConfidenceLevelLowSizeSmall { confidenceLevelLowSizeSmall = { text = Locale.string vc.locale "Low" } }


val : View.Config -> String -> { firstRow : String, secondRow : String, secondRowVisible : Bool }
val vc str =
    { firstRow = Locale.string vc.locale str
    , secondRow = ""
    , secondRowVisible = False
    }


baseRowStyle : List Css.Style
baseRowStyle =
    [ Css.width (Css.pct 100), Css.fontSize (Css.px 14) ]


row : { tooltipRowLabel : { title : String }, tooltipRowValue : { firstRow : String, secondRowVisible : Bool, secondRow : String } } -> Html msg
row =
    GraphComponents.tooltipRowWithAttributes
        (GraphComponents.tooltipRowAttributes
            |> Rs.s_tooltipRow [ css baseRowStyle ]
        )


showActor : View.Config -> Actor -> List (Html msg)
showActor vc a =
    let
        mainUri =
            if not (String.startsWith "http://" a.uri) || not (String.startsWith "https://" a.uri) then
                "https://" ++ a.uri

            else
                a.uri
    in
    [ row
        { tooltipRowLabel = { title = Locale.string vc.locale "Actor" }
        , tooltipRowValue = a.label |> val vc
        }
    , GraphComponents.tooltipRowWithInstances
        (GraphComponents.tooltipRowAttributes
            |> Rs.s_tooltipRow [ css baseRowStyle ]
        )
        (GraphComponents.tooltipRowInstances
            |> Rs.s_tooltipRowValue
                (Just (Html.Styled.a [ Css.plainLinkStyle vc |> css, href mainUri, target "blank" ] [ text a.uri ]))
        )
        { tooltipRowLabel = { title = Locale.string vc.locale "Url" }
        , tooltipRowValue = { firstRow = "", secondRow = "", secondRowVisible = False }
        }
    , GraphComponents.tooltipRowWithInstances
        (GraphComponents.tooltipRowAttributes
            |> Rs.s_tooltipRow [ css baseRowStyle ]
        )
        (GraphComponents.tooltipRowInstances
            |> Rs.s_tooltipRowValue
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
                    |> Just
                )
        )
        { tooltipRowLabel = { title = Locale.string vc.locale "Jurisdictions" }
        , tooltipRowValue = { firstRow = "", secondRow = "", secondRowVisible = False }
        }
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
    [ GraphComponents.tooltipRowWithInstances
        (GraphComponents.tooltipRowAttributes
            |> Rs.s_tooltipRow [ css baseRowStyle ]
        )
        (GraphComponents.tooltipRowInstances
            |> Rs.s_tooltipRowValue
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
                    |> Just
                )
        )
        { tooltipRowLabel = { title = Locale.string vc.locale "Labels" }
        , tooltipRowValue = { firstRow = "", secondRow = "", secondRowVisible = False }
        }
    , GraphComponents.tooltipRowWithInstances
        (GraphComponents.tooltipRowAttributes
            |> Rs.s_tooltipRow [ css baseRowStyle ]
        )
        (GraphComponents.tooltipRowInstances
            |> Rs.s_tooltipRowValue
                (getConfidenceIndicator vc maxConfidence |> Just)
        )
        { tooltipRowLabel = { title = Locale.string vc.locale "Confidence" }
        , tooltipRowValue = { firstRow = "", secondRow = "", secondRowVisible = False }
        }
    , row
        { tooltipRowLabel = { title = Locale.string vc.locale "Sources" }
        , tooltipRowValue =
            Set.size sources
                |> String.fromInt
                |> val vc
        }
    , GraphComponents.tooltipRowWithInstances
        (GraphComponents.tooltipRowAttributes
            |> Rs.s_tooltipRow [ css baseRowStyle ]
        )
        (GraphComponents.tooltipRowInstances
            |> Rs.s_tooltipRowValue
                (let
                    btn =
                        Buttons.buttonTypeTextStateRegularStyleTextWithAttributes
                            (Buttons.buttonTypeTextStateRegularStyleTextAttributes
                                |> Rs.s_button
                                    (case openDetailsMsg of
                                        Just m ->
                                            [ [ Css.cursor Css.pointer ] |> css, onClick m ]

                                        _ ->
                                            [ [ Css.display Css.none ] |> css ]
                                    )
                            )
                            { typeTextStateRegularStyleText =
                                { buttonText = Locale.string vc.locale "Learn more"
                                , iconInstance = none
                                , iconVisible = False
                                }
                            }
                 in
                 btn
                    |> Just
                )
        )
        { tooltipRowLabel = { title = "" }
        , tooltipRowValue = { firstRow = "", secondRow = "", secondRowVisible = False }
        }

    -- , row
    --     { tooltipRowLabel = { title = Locale.string vc.locale "Mentions" }
    --     , tooltipRowValue = tagCount |> String.fromInt |> val vc
    --     }
    ]


tagLabel : View.Config -> String -> TagSummary -> List (Html msg)
tagLabel vc lbl tag =
    let
        mlbldata =
            Dict.get lbl tag.labelSummary
    in
    case mlbldata of
        Just lbldata ->
            [ row
                { tooltipRowLabel = { title = Locale.string vc.locale "Tag label" }
                , tooltipRowValue = lbldata.label |> val vc
                }
            , GraphComponents.tooltipRowWithInstances
                (GraphComponents.tooltipRowAttributes
                    |> Rs.s_tooltipRow [ css baseRowStyle ]
                )
                (GraphComponents.tooltipRowInstances
                    |> Rs.s_tooltipRowValue
                        (getConfidenceIndicator vc lbldata.confidence |> Just)
                )
                { tooltipRowLabel = { title = Locale.string vc.locale "Confidence" }
                , tooltipRowValue = { firstRow = "", secondRow = "", secondRowVisible = False }
                }
            ]
                ++ (if List.isEmpty lbldata.concepts then
                        []

                    else
                        row
                            { tooltipRowLabel = { title = Locale.string vc.locale "Categories" }
                            , tooltipRowValue =
                                lbldata.concepts
                                    |> List.map (\x -> getConceptName vc (Just x) |> Maybe.withDefault x)
                                    |> String.join ", "
                                    |> Locale.string vc.locale
                                    |> Util.View.truncate 20
                                    |> val vc
                            }
                            |> List.singleton
                   )
                ++ [ row
                        { tooltipRowLabel = { title = Locale.string vc.locale "Sources" }
                        , tooltipRowValue =
                            List.length lbldata.sources
                                |> String.fromInt
                                |> val vc
                        }
                   , row
                        { tooltipRowLabel = { title = Locale.string vc.locale "Mentions" }
                        , tooltipRowValue = lbldata.count |> String.fromInt |> val vc
                        }
                   , row
                        { tooltipRowLabel = { title = Locale.string vc.locale "Last modified" }
                        , tooltipRowValue =
                            let
                                date =
                                    Locale.timestampDateUniform vc.locale lbldata.lastmod

                                time =
                                    Locale.timestampTimeUniform vc.locale vc.showTimeZoneOffset lbldata.lastmod
                            in
                            { firstRow = date
                            , secondRow = time
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
    [ row
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
    , row
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
    , row
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
                    [ row
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
    [ row
        { tooltipRowLabel = { title = Locale.string vc.locale "Tx hash" }
        , tooltipRowValue =
            tx.txId
                |> truncateLongIdentifierWithLengths 8 4
                |> val vc
        }
    , row
        { tooltipRowLabel = { title = Locale.string vc.locale "Timestamp" }
        , tooltipRowValue =
            let
                date =
                    Locale.timestampDateUniform vc.locale tx.timestamp

                time =
                    Locale.timestampTimeUniform vc.locale vc.showTimeZoneOffset tx.timestamp
            in
            { firstRow = date
            , secondRow = time
            , secondRowVisible = True
            }
        }
    ]

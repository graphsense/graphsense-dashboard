module Util.Tooltip exposing (linkRow, tooltipConfig, tooltipRow, tooltipRowCustomValue, view)

import Api.Data exposing (Actor, TagSummary)
import Basics.Extra exposing (flip)
import Components.Tooltip as Tooltip
import Config.View as View exposing (getConceptName)
import Css
import Css.Pathfinder as Css
import Dict
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes exposing (css, href, target, title)
import Model.Currency exposing (assetFromBase)
import Model.Pathfinder as Pathfinder exposing (getTagSummary)
import Model.Pathfinder.Address as Addr
import Model.Pathfinder.Id as Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import RecordSetter as Rs
import Set
import Theme.Html.GraphComponents as GraphComponents
import Theme.Html.TagsComponents as TagsComponents
import Tuple exposing (pair)
import Util.Css as Css
import Util.Data as Data
import Util.Pathfinder.TagConfidence exposing (ConfidenceRange(..), getConfidenceRangeFromFloat)
import Util.Pathfinder.TagSummary as TagSummary
import Util.TooltipType exposing (TooltipType(..))
import Util.View exposing (makeValuesList, truncateLongIdentifier, truncateLongIdentifierWithLengths)
import View.Button as Button
import View.Locale as Locale
import Msg.Pathfinder exposing (OverlayWindows(..))


tooltipConfig : View.Config -> (Tooltip.Msg TooltipType -> msg) -> Tooltip.Config TooltipType msg
tooltipConfig vc tag =
    Tooltip.defaultConfig tag
        |> Tooltip.withZIndex (Css.zIndexMainValue + 10000)
        |> Tooltip.withBorderColor (vc.theme.hovercard vc.lightmode).borderColor
        |> Tooltip.withBackgroundColor (vc.theme.hovercard vc.lightmode).backgroundColor
        |> Tooltip.withBorderWidth (vc.theme.hovercard vc.lightmode).borderWidth
        |> Tooltip.withCloseDelay 100
        |> Tooltip.withFixed


view : View.Config -> Pathfinder.Model -> TooltipType -> Html Msg
view vc model tt =
    div [] <|
        case tt of
            UtxoTx t ->
                genericTx vc { txId = t.raw.txHash, timestamp = t.raw.timestamp }

            AccountTx t ->
                genericTx vc { txId = t.raw.identifier, timestamp = t.raw.timestamp }

            AggEdge a ->
                aggEdge vc a

            Address a ts ->
                address vc ts a

            TagLabel lblid x ->
                tagLabel vc lblid x

            TagConcept id conceptId ->
                getTagSummary model id
                    |> Maybe.map (tagConcept vc id conceptId)
                    |> Maybe.withDefault [ Html.text "no tagsummary found" ]

            ActorDetails ac ->
                showActor vc ac

            ChangeHeuristics cfg ->
                changeHeuristics vc cfg

            Text txt ->
                Locale.string vc.locale txt
                    |> Html.text
                    |> List.singleton


getConfidenceIndicator : View.Config -> Float -> Html msg
getConfidenceIndicator vc x =
    let
        r =
            getConfidenceRangeFromFloat x

        lbl =
            case r of
                High ->
                    "high confidence"

                Medium ->
                    "medium confidence"

                Low ->
                    "low confidence"

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
    [ Css.width (Css.pct 100) ]


tooltipRow : { tooltipRowLabel : { title : String }, tooltipRowValue : { firstRowText : String, secondRowVisible : Bool, secondRowText : String } } -> Html msg
tooltipRow =
    GraphComponents.tooltipRowWithAttributes
        (GraphComponents.tooltipRowAttributes
            |> Rs.s_root [ css baseRowStyle ]
            |> Rs.s_tooltipRowLabel [ css [ Css.minWidth (Css.px 90) ] ]
            |> Rs.s_firstValue [ css [ Css.property "white-space" "wrap", Css.textAlign Css.right ] ]
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
        { tooltipRowLabel = { title = Locale.string vc.locale "actor" }
        , tooltipRowValue = a.label |> val vc
        }
    , tooltipRowCustomValue (Locale.string vc.locale "Url") (Html.a [ Css.plainLinkStyle vc |> css, href mainUri, target "blank" ] [ text a.uri ])
    , tooltipRowCustomValue
        (Locale.string vc.locale "jurisdictions")
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


tagConcept : View.Config -> Id -> String -> TagSummary -> List (Html Msg)
tagConcept vc id concept tag =
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
    [ tooltipRow
        { tooltipRowLabel =
            { title = Locale.string vc.locale "Labels"
            }
        , tooltipRowValue =
            let
                max_labels =
                    7
            in
            labels
                |> List.take max_labels
                |> List.intersperse ", "
                |> flip (++)
                    (if List.length labels > max_labels then
                        [ "..." ]

                     else
                        []
                    )
                |> String.concat
                |> val vc
        }
    , tooltipRowCustomValue (Locale.string vc.locale "confidence") (getConfidenceIndicator vc maxConfidence)
    , tooltipRow
        { tooltipRowLabel = { title = Locale.string vc.locale "sources" }
        , tooltipRowValue =
            Set.size sources
                |> String.fromInt
                |> val vc
        }
    , TagsList id |> UserOpensDialogWindow |> linkRow vc "learn more"
    ]


changeHeuristics : View.Config -> { confidence : Float, heuristics : List String } -> List (Html msg)
changeHeuristics vc cfg =
    let
        maxItems =
            7

        labels =
            cfg.heuristics
                |> List.filter (String.isEmpty >> not)

        more =
            List.length labels - maxItems

        labelsPreview =
            labels
                |> List.take maxItems
                |> List.intersperse ", "
                |> flip (++)
                    (if more > 0 then
                        [ "..." ]

                     else
                        []
                    )
                |> String.concat
    in
    [ tooltipRow
        { tooltipRowLabel =
            { title = Locale.string vc.locale "Type"
            }
        , tooltipRowValue =
            labelsPreview
                |> val vc
        }
    , tooltipRowCustomValue (Locale.string vc.locale "confidence") (getConfidenceIndicator vc cfg.confidence)
    , tooltipRow
        { tooltipRowLabel = { title = Locale.string vc.locale "sources" }
        , tooltipRowValue =
            List.length labels
                |> String.fromInt
                |> val vc
        }
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
                { tooltipRowLabel = { title = Locale.string vc.locale "tag Label" }
                , tooltipRowValue = lbldata.label |> val vc
                }
            , tooltipRowCustomValue (Locale.string vc.locale "confidence") (getConfidenceIndicator vc lbldata.confidence)
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
                        { tooltipRowLabel = { title = Locale.string vc.locale "sources" }
                        , tooltipRowValue =
                            List.length lbldata.sources
                                |> String.fromInt
                                |> val vc
                        }
                   , tooltipRow
                        { tooltipRowLabel = { title = Locale.string vc.locale "mentions" }
                        , tooltipRowValue = lbldata.count |> String.fromInt |> val vc
                        }
                   , tooltipRow
                        { tooltipRowLabel = { title = Locale.string vc.locale "Last modified" }
                        , tooltipRowValue =
                            let
                                t =
                                    Data.timestampToPosix lbldata.lastmod

                                date =
                                    Locale.timestampDateUniform vc.locale t

                                time =
                                    Locale.timestampTimeUniform vc.locale vc.showTimeZoneOffset t
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

        curr =
            View.toCurrency vc
    in
    [ tooltipRow
        { tooltipRowLabel = { title = Locale.string vc.locale "Balance" }
        , tooltipRowValue =
            Addr.getBalance adr
                |> Maybe.map
                    (pair (assetFromBase net)
                        >> List.singleton
                        >> Locale.currency curr vc.locale
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
                        >> Locale.currency curr vc.locale
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
                        >> Locale.currency curr vc.locale
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
                t =
                    Data.timestampToPosix tx.timestamp

                date =
                    Locale.timestampDateUniform vc.locale t

                time =
                    Locale.timestampTimeUniform vc.locale vc.showTimeZoneOffset t
            in
            { firstRowText = date
            , secondRowText = time
            , secondRowVisible = True
            }
        }
    ]


aggEdge : View.Config -> { leftAddress : Id, left : Maybe Api.Data.NeighborAddress, rightAddress : Id, right : Maybe Api.Data.NeighborAddress } -> List (Html msg)
aggEdge vc { leftAddress, left, rightAddress, right } =
    [ GraphComponents.tooltipRowIcon2ValuesWithAttributes
        (GraphComponents.tooltipRowIcon2ValuesAttributes
            |> Rs.s_root [ css [ Css.width (Css.px 250) ] ]
        )
        { tooltipRowValue0 =
            leftAddress
                |> Id.id
                |> truncateLongIdentifier
                |> val vc
        , tooltipRowValue1 =
            rightAddress
                |> Id.id
                |> truncateLongIdentifier
                |> val vc
        }
    , GraphComponents.tooltipRow2ValuesWithAttributes
        (GraphComponents.tooltipRow2ValuesAttributes
            |> Rs.s_root [ css baseRowStyle ]
        )
        { tooltipRowValue1 =
            left
                |> Maybe.map .noTxs
                |> Maybe.withDefault 0
                |> Locale.int vc.locale
                |> val vc
        , tooltipRowValue2 =
            right
                |> Maybe.map .noTxs
                |> Maybe.withDefault 0
                |> Locale.int vc.locale
                |> val vc
        , tooltipRowLabel = { title = Locale.string vc.locale "transactions" }
        }
    ]
        ++ (let
                valuesList =
                    makeValuesList vc (Id.network leftAddress) right left

                maxLen =
                    4

                more =
                    List.length valuesList - maxLen
            in
            valuesList
                |> List.take maxLen
                |> List.map
                    (\{ leftValue, rightValue } ->
                        GraphComponents.tooltipRow2ValuesWithAttributes
                            (GraphComponents.tooltipRow2ValuesAttributes
                                |> Rs.s_root [ css baseRowStyle ]
                            )
                            { tooltipRowValue1 =
                                (if not vc.showValuesInFiat then
                                    leftValue.value
                                        |> Locale.coinWithoutCode vc.locale leftValue.asset

                                 else
                                    leftValue.fiat
                                )
                                    |> val vc
                            , tooltipRowValue2 =
                                (if not vc.showValuesInFiat then
                                    rightValue.value
                                        |> Locale.coinWithoutCode vc.locale rightValue.asset

                                 else
                                    rightValue.fiat
                                )
                                    |> val vc
                            , tooltipRowLabel = { title = String.toUpper leftValue.asset.asset }
                            }
                    )
                |> flip (++)
                    (if more > 0 then
                        [ GraphComponents.tooltipRow2ValuesWithAttributes
                            (GraphComponents.tooltipRow2ValuesAttributes
                                |> Rs.s_root [ css baseRowStyle ]
                            )
                            { tooltipRowValue1 = val vc ""
                            , tooltipRowValue2 = val vc ""
                            , tooltipRowLabel =
                                { title =
                                    "+ "
                                        ++ Locale.interpolated vc.locale
                                            (if more > 1 then
                                                "Num-more-assets"

                                             else
                                                "one more asset"
                                            )
                                            [ String.fromInt more ]
                                }
                            }
                        ]

                     else
                        []
                    )
           )

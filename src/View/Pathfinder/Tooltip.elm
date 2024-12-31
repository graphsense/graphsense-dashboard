module View.Pathfinder.Tooltip exposing (view)

import Api.Data exposing (Actor, TagSummary)
import Config.View as View exposing (getConceptName)
import Css
import Css.Pathfinder as Css
import Dict exposing (Dict)
import Html.Styled exposing (Html, div, span, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href, target, title)
import Html.Styled.Events exposing (onMouseEnter, onMouseLeave)
import Model.Currency exposing (assetFromBase)
import Model.Pathfinder exposing (HavingTags(..))
import Model.Pathfinder.Address as Addr
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Tooltip exposing (Tooltip, TooltipType(..))
import Msg.Pathfinder exposing (Msg(..))
import RecordSetter as Rs
import Theme.Html.GraphComponents as GraphComponents
import Theme.Html.TagsComponents as TagComponents
import Tuple exposing (pair)
import Util.Css as Css
import Util.Pathfinder.TagConfidence exposing (ConfidenceRange(..), getConfidenceRangeFromFloat)
import Util.Pathfinder.TagSummary as TagSummary
import Util.View exposing (hovercard, truncateLongIdentifierWithLengths)
import View.Locale as Locale


view : View.Config -> Dict Id HavingTags -> Tooltip -> Html Msg
view vc ts tt =
    let
        ( content, containerAttributes ) =
            case tt.type_ of
                UtxoTx t ->
                    ( genericTx vc { txId = t.raw.txHash, timestamp = t.raw.timestamp }, [] )

                AccountTx t ->
                    ( genericTx vc { txId = t.raw.identifier, timestamp = t.raw.timestamp }, [] )

                Address a ->
                    ( address vc (Dict.get a.id ts) a, [] )

                TagLabel lblid x ->
                    ( tagLabel vc lblid x, [ onMouseEnter (UserMovesMouseOverTagLabel lblid), onMouseLeave (UserMovesMouseOutTagLabel lblid) ] )

                ActorDetails ac ->
                    ( showActor vc ac, [ onMouseEnter (UserMovesMouseOverActorLabel ac.id), onMouseLeave (UserMovesMouseOutActorLabel ac.id) ] )
    in
    content
        |> div
            (css GraphComponents.tooltipProperty1Down_details.styles
                :: containerAttributes
            )
        |> toUnstyled
        |> List.singleton
        |> hovercard vc tt.hovercard (Css.zIndexMainValue + 1)


getConfidenceIndicator : View.Config -> Float -> Html Msg
getConfidenceIndicator vc x =
    let
        r =
            getConfidenceRangeFromFloat x
    in
    case r of
        High ->
            TagComponents.confidenceLevelConfidenceLevelHigh { confidenceLevelHigh = { text = Locale.string vc.locale "High" } }

        Medium ->
            TagComponents.confidenceLevelConfidenceLevelMedium { confidenceLevelMedium = { text = Locale.string vc.locale "Medium" } }

        Low ->
            TagComponents.confidenceLevelConfidenceLevelLow { confidenceLevelLow = { text = Locale.string vc.locale "Low" } }


val : View.Config -> String -> { firstRow : String, secondRow : String, secondRowVisible : Bool }
val vc str =
    { firstRow = Locale.string vc.locale str
    , secondRow = ""
    , secondRowVisible = False
    }


row : { tooltipRowLabel : { title : String }, tooltipRowValue : { firstRow : String, secondRowVisible : Bool, secondRow : String } } -> Html Msg
row =
    GraphComponents.tooltipRowWithAttributes
        (GraphComponents.tooltipRowAttributes
            |> Rs.s_tooltipRow [ css [ Css.width (Css.pct 100) ] ]
        )


showActor : View.Config -> Actor -> List (Html Msg)
showActor vc a =
    [ row
        { tooltipRowLabel = { title = Locale.string vc.locale "Actor" }
        , tooltipRowValue = a.label |> val vc
        }
    , GraphComponents.tooltipRowWithInstances
        (GraphComponents.tooltipRowAttributes
            |> Rs.s_tooltipRow [ css [ Css.width (Css.pct 100) ] ]
        )
        (GraphComponents.tooltipRowInstances
            |> Rs.s_tooltipRowValue
                (Just (Html.Styled.a [ Css.plainLinkStyle vc |> css, href a.uri, target "blank" ] [ text a.uri ]))
        )
        { tooltipRowLabel = { title = Locale.string vc.locale "Url" }
        , tooltipRowValue = { firstRow = "", secondRow = "", secondRowVisible = False }
        }
    , GraphComponents.tooltipRowWithInstances
        (GraphComponents.tooltipRowAttributes
            |> Rs.s_tooltipRow [ css [ Css.width (Css.pct 100) ] ]
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
                            span
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
                    |> div []
                    |> Just
                )
        )
        { tooltipRowLabel = { title = Locale.string vc.locale "Jurisdictions" }
        , tooltipRowValue = { firstRow = "", secondRow = "", secondRowVisible = False }
        }
    ]


tagLabel : View.Config -> String -> TagSummary -> List (Html Msg)
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
                    |> Rs.s_tooltipRow [ css [ Css.width (Css.pct 100) ] ]
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
                                    |> String.join ","
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


address : View.Config -> Maybe HavingTags -> Addr.Address -> List (Html Msg)
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
                Just (HasTagSummary ts) ->
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


genericTx : View.Config -> { txId : String, timestamp : Int } -> List (Html Msg)
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

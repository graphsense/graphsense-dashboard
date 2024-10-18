module View.Pathfinder.Tooltip exposing (view)

import Api.Data exposing (Actor, TagSummary)
import Config.View as View
import Css
import Css.Pathfinder as Css
import Dict exposing (Dict)
import Html.Styled exposing (Html, div, span, text, toUnstyled)
import Html.Styled.Attributes exposing (css, title)
import Model.Currency exposing (assetFromBase)
import Model.Pathfinder exposing (HavingTags(..))
import Model.Pathfinder.Address as Addr
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Tooltip exposing (Tooltip, TooltipType(..))
import RecordSetter as Rs
import Theme.Html.GraphComponents as GraphComponents
import Tuple exposing (pair)
import Util.Css as Css
import Util.Flags exposing (getFlagEmoji)
import Util.Pathfinder.TagSummary as TagSummary
import Util.View exposing (hovercard, none, truncateLongIdentifierWithLengths)
import View.Locale as Locale


view : View.Config -> Dict Id HavingTags -> Tooltip -> Html msg
view vc ts tt =
    (case tt.type_ of
        UtxoTx t ->
            genericTx vc { txId = t.raw.txHash, timestamp = t.raw.timestamp }

        AccountTx t ->
            genericTx vc { txId = t.raw.identifier, timestamp = t.raw.timestamp }

        Address a ->
            address vc (Dict.get a.id ts) a

        TagLabel lblid x ->
            tagLabel vc lblid x

        ActorDetails ac ->
            showActor vc ac
    )
        |> List.singleton
        |> div [ Css.tooltipMargin |> css ]
        |> toUnstyled
        |> List.singleton
        |> hovercard vc tt.hovercard (Css.zIndexMainValue + 1)


getConfidenceIndicator : View.Config -> Float -> Html msg
getConfidenceIndicator vc x =
    if x >= 0.8 then
        span [ Css.tagConfidenceTextHighStyle vc |> css ] [ Locale.text vc.locale "High" ]

    else if x >= 0.4 then
        span [ Css.tagConfidenceTextMediumStyle vc |> css ] [ Locale.text vc.locale "Medium" ]

    else
        span [ Css.tagConfidenceTextLowStyle vc |> css ] [ Locale.text vc.locale "Low" ]


val : View.Config -> String -> { firstRow : String, secondRow : String, secondRowVisible : Bool }
val vc str =
    { firstRow = Locale.string vc.locale str
    , secondRow = ""
    , secondRowVisible = False
    }


row : { tooltipRowLabel : { title : String }, tooltipRowValue : { firstRow : String, secondRowVisible : Bool, secondRow : String } } -> Html msg
row =
    GraphComponents.tooltipRowWithAttributes
        (GraphComponents.tooltipRowAttributes
            |> Rs.s_tooltipRow [ css [ Css.width (Css.pct 100) ] ]
        )


showActor : View.Config -> Actor -> Html msg
showActor vc a =
    div []
        [ row
            { tooltipRowLabel = { title = Locale.string vc.locale "Actor" }
            , tooltipRowValue = a.label |> val vc
            }
        , row
            { tooltipRowLabel = { title = Locale.string vc.locale "Url" }
            , tooltipRowValue = a.uri |> val vc
            }
        , GraphComponents.tooltipRowWithInstances
            (GraphComponents.tooltipRowAttributes
                |> Rs.s_tooltipRow [ css [ Css.width (Css.pct 100) ] ]
            )
            (GraphComponents.tooltipRowInstances
                |> Rs.s_tooltipRowValue
                    (a.jurisdictions
                        |> List.map
                            (\z ->
                                span
                                    [ title (Locale.string vc.locale z.label)
                                    , Css.mGap
                                        |> Css.paddingRight
                                        |> List.singleton
                                        |> css
                                    ]
                                    [ text (getFlagEmoji z.id) ]
                            )
                        |> div []
                        |> Just
                    )
            )
            { tooltipRowLabel = { title = Locale.string vc.locale "Jurisdictions" }
            , tooltipRowValue = { firstRow = "", secondRow = "", secondRowVisible = False }
            }
        ]


tagLabel : View.Config -> String -> TagSummary -> Html msg
tagLabel vc lbl tag =
    let
        mlbldata =
            Dict.get lbl tag.labelSummary
    in
    case mlbldata of
        Just lbldata ->
            div
                []
                ([ row
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
                )

        _ ->
            none


address : View.Config -> Maybe HavingTags -> Addr.Address -> Html msg
address vc tags adr =
    let
        net =
            Id.network adr.id
    in
    div
        []
        ([ row
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
        )


genericTx : View.Config -> { txId : String, timestamp : Int } -> Html msg
genericTx vc tx =
    div
        []
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

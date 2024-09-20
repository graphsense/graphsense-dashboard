module View.Pathfinder.Tooltip exposing (view)

import Api.Data exposing (TagSummary)
import Config.View as View
import Css
import Css.Pathfinder as Css
import Dict exposing (Dict)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Model.Currency exposing (assetFromBase)
import Model.Pathfinder exposing (HavingTags)
import Model.Pathfinder.Address as Address
import Model.Pathfinder.Id as Id
import Model.Pathfinder.Tooltip exposing (Tooltip, TooltipType(..))
import RecordSetter exposing (..)
import Theme.Html.GraphComponents as GraphComponents
import Tuple exposing (pair)
import Util.Css as Css
import Util.View exposing (hovercard, none, truncateLongIdentifierWithLengths)
import View.Locale as Locale


view : View.Config -> Dict Id.Id HavingTags -> Tooltip -> Html msg
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
    )
        |> List.singleton
        |> div [ Css.tooltipMargin |> css ]
        |> Html.toUnstyled
        |> List.singleton
        |> hovercard vc tt.hovercard (Css.zIndexMainValue + 1)


getConfidenceIndicator : View.Config -> Float -> Html msg
getConfidenceIndicator vc x =
    if x >= 0.8 then
        span [ css [ Css.color (Css.successColor vc) ] ] [ Locale.text vc.locale "High" ]

    else if x >= 0.4 then
        span [ css [ Css.color (Css.warningColor vc) ] ] [ Locale.text vc.locale "Medium" ]

    else
        span [ css [ Css.color (Css.alertColor vc) ] ] [ Locale.text vc.locale "Low" ]


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
            |> s_tooltipRow [ css [ Css.width (Css.pct 100) ] ]
        )


tagLabel : View.Config -> String -> TagSummary -> Html msg
tagLabel vc lbl tag =
    let
        mlbldata =
            Dict.get lbl tag.labelSummary
    in
    case mlbldata of
        Just lbldata ->
            div
                [--css tooltipBaseCss
                ]
                [ row
                    { tooltipRowLabel = { title = Locale.string vc.locale "Tag label" }
                    , tooltipRowValue = lbldata.label |> val vc
                    }
                , GraphComponents.tooltipRowWithInstances
                    (GraphComponents.tooltipRowAttributes
                        |> s_tooltipRow [ css [ Css.width (Css.pct 100) ] ]
                    )
                    (GraphComponents.tooltipRowInstances
                        |> s_tooltipRowValue
                            (getConfidenceIndicator vc lbldata.confidence |> Just)
                    )
                    { tooltipRowLabel = { title = Locale.string vc.locale "Confidence" }
                    , tooltipRowValue = { firstRow = "", secondRow = "", secondRowVisible = False }
                    }
                , row
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
            none


address : View.Config -> Maybe HavingTags -> Address.Address -> Html msg
address vc _ adr =
    let
        net =
            Id.network adr.id
    in
    div
        []
        [ row
            { tooltipRowLabel = { title = Locale.string vc.locale "Balance" }
            , tooltipRowValue =
                Address.getBalance adr
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
                Address.getTotalReceived adr
                    |> Maybe.map
                        (pair (assetFromBase net)
                            >> List.singleton
                            >> Locale.currency vc.locale
                        )
                    |> Maybe.withDefault ""
                    |> val vc
            }
        ]


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

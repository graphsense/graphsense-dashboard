module View.Pathfinder.Tooltip exposing (view)

import Api.Data exposing (TagSummary)
import Config.Graph exposing (AddressLabelType(..))
import Config.View as View
import Css
import Css.Pathfinder as Css
import Css.View as Css
import Dict exposing (Dict)
import Hovercard
import Html.Attributes
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css, title)
import Model.Currency exposing (assetFromBase)
import Model.Pathfinder exposing (HavingTags(..))
import Model.Pathfinder.Address as Address
import Model.Pathfinder.Id as Id
import Model.Pathfinder.Tooltip exposing (Tooltip, TooltipType(..))
import Model.Pathfinder.Tx as Tx
import RecordSetter exposing (..)
import RemoteData exposing (WebData)
import Theme.Html.GraphComponents as GraphComponents exposing (tooltipProperty1DownAttributes)
import Util.Css as Css
import Util.View exposing (hovercard, none, truncateLongIdentifierWithLengths)
import View.Locale as Locale
import View.Pathfinder.Utils exposing (multiLineDateTimeFromTimestamp)


view : View.Config -> Dict Id.Id HavingTags -> Tooltip -> Html msg
view vc ts tt =
    (case tt.type_ of
        UtxoTx t ->
            utxoTx vc t

        Address a ->
            address vc (Dict.get a.id ts) a

        TagLabel lblid x ->
            tagLabel vc lblid x
    )
        |> Html.toUnstyled
        |> List.singleton
        |> hovercard vc tt.hovercard (Css.zIndexMainValue + 1)



{-
   tooltipBaseCss : List Css.Style
   tooltipBaseCss =
       GraphComponents.tooltipProperty1DownDetails.styles ++ [ Css.width Css.auto, Css.minWidth (Css.px 100) ]
-}


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


tagLabel : View.Config -> String -> TagSummary -> Html msg
tagLabel vc lbl tag =
    let
        mlbldata =
            Dict.get lbl tag.labelSummary

        row =
            GraphComponents.tooltipRowWithAttributes
                (GraphComponents.tooltipRowAttributes
                    |> s_tooltipRow [ css [ Css.width (Css.pct 100) ] ]
                )
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
address vc havingTags adr =
    let
        net =
            Id.network adr.id

        balance =
            Address.getBalance adr |> Maybe.map .value |> Maybe.map (Locale.coinWithoutCode vc.locale (assetFromBase net)) |> Maybe.withDefault ""

        totalReceived =
            Address.getTotalReceived adr |> Maybe.map .value |> Maybe.map (Locale.coinWithoutCode vc.locale (assetFromBase net)) |> Maybe.withDefault ""

        currency =
            Address.getCurrency adr |> Maybe.map String.toUpper |> Maybe.withDefault ""
    in
    Debug.todo """
    div
        [ css tooltipBaseCss
        ]
        [ div
            [ css GraphComponents.tooltipProperty1DownContent1Details.styles
            , css [ Css.whiteSpace Css.noWrap ]
            ]
            [ key vc "Balance"
            , key vc "Total Received"
            ]
        , div
            [ css GraphComponents.tooltipProperty1DownContent2Details.styles
            , css [ Css.whiteSpace Css.noWrap ]
            ]
            [ balance
                ++ " "
                ++ currency
                |> text
                |> val
            , totalReceived
                ++ " "
                ++ currency
                |> text
                |> val
            ]
        ]
        """


utxoTx : View.Config -> Tx.UtxoTx -> Html msg
utxoTx vc tx =
    Debug.todo """
    div
        [ css tooltipBaseCss
        ]
        [ div
            [ css GraphComponents.tooltipProperty1DownContent1Details.styles
            , css [ Css.whiteSpace Css.noWrap ]
            ]
            [ key vc "Tx hash"
            , key vc "Timestamp"
            ]
        , div
            [ css GraphComponents.tooltipProperty1DownContent2Details.styles
            , css [ Css.whiteSpace Css.noWrap ]
            ]
            [ tx.raw.txHash
                |> truncateLongIdentifierWithLengths 8 4
                |> text
                |> val
            , tx.raw.timestamp
                |> multiLineDateTimeFromTimestamp vc
                |> val
            ]
        ]
        """

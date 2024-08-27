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
import Html.Styled.Attributes exposing (css)
import Model.Currency exposing (assetFromBase)
import Model.Pathfinder exposing (HavingTags(..))
import Model.Pathfinder.Address as Address
import Model.Pathfinder.Id as Id
import Model.Pathfinder.Tooltip exposing (Tooltip, TooltipType(..))
import Model.Pathfinder.Tx as Tx
import RemoteData exposing (WebData)
import Theme.Html.GraphComponents as GraphComponents exposing (tooltipProperty1DownAttributes)
import Util.Css as Css
import Util.View exposing (none, truncateLongIdentifierWithLengths)
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
        |> Hovercard.view
            { tickLength = 16
            , zIndex = Css.zIndexMainValue + 1
            , borderColor = (vc.theme.hovercard vc.lightmode).borderColor
            , backgroundColor = (vc.theme.hovercard vc.lightmode).backgroundColor
            , borderWidth = (vc.theme.hovercard vc.lightmode).borderWidth
            , viewport = vc.size
            }
            tt.hovercard
            (Css.hovercard vc
                |> List.map (\( k, v ) -> Html.Attributes.style k v)
            )
        |> Html.fromUnstyled


getConfidenceIndicator : View.Config -> Float -> Html msg
getConfidenceIndicator vc x =
    if x >= 0.8 then
        span [ css [ Css.color (Css.successColor vc) ] ] [ Locale.text vc.locale "High" ]

    else if x >= 0.4 then
        span [ css [ Css.color (Css.warningColor vc) ] ] [ Locale.text vc.locale "Medium" ]

    else
        span [ css [ Css.color (Css.alertColor vc) ] ] [ Locale.text vc.locale "Low" ]


key : View.Config -> String -> Html msg
key vc =
    Locale.string vc.locale
        >> text
        >> List.singleton
        >> div
            [ css GraphComponents.tooltipProperty1DownLabel1Details.styles
            ]


val : Html msg -> Html msg
val =
    List.singleton
        >> div
            [ css GraphComponents.tooltipProperty1DownValue1Details.styles
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
                [ css GraphComponents.tooltipProperty1DownDetails.styles
                ]
                [ div
                    [ css GraphComponents.tooltipProperty1DownContent1Details.styles
                    , css [ Css.whiteSpace Css.noWrap ]
                    ]
                    [ key vc "Tag Label"
                    , key vc "Confidence"
                    , key vc "Sources"
                    , key vc "Mentions"
                    , key vc "Last Modified"
                    ]
                , div
                    [ css GraphComponents.tooltipProperty1DownContent2Details.styles
                    , css [ Css.whiteSpace Css.noWrap ]
                    ]
                    [ lbldata.label
                        |> text
                        |> val
                    , getConfidenceIndicator vc lbldata.confidence |> val
                    , List.length lbldata.sources
                        |> String.fromInt
                        |> text
                        |> val
                    , lbldata.count
                        |> String.fromInt
                        |> text
                        |> val
                    , lbldata.lastmod
                        |> multiLineDateTimeFromTimestamp vc
                        |> val
                    ]
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
    div
        [ css GraphComponents.tooltipProperty1DownDetails.styles
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


utxoTx : View.Config -> Tx.UtxoTx -> Html msg
utxoTx vc tx =
    div
        [ css GraphComponents.tooltipProperty1DownDetails.styles
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

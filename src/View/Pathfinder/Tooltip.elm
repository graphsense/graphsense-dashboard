module View.Pathfinder.Tooltip exposing (view)

import Api.Data exposing (TagSummary)
import Config.Graph exposing (AddressLabelType(..))
import Config.View as View
import Css
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


view : View.Config -> Dict Id.Id HavingTags -> Tooltip -> Html msg
view vc ts tt =
    (case tt.type_ of
        UtxoTx t ->
            utxoTx vc t

        Address a ->
            address vc (Dict.get a.id ts) a
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


address : View.Config -> Maybe HavingTags -> Address.Address -> Html msg
address vc havingTags adr =
    let
        net =
            Id.network adr.id

        ts =
            case havingTags of
                Just (HasTagSummary t) ->
                    Just t

                _ ->
                    Nothing

        category =
            ts |> Maybe.map .broadCategory |> Maybe.withDefault "-"

        lbl =
            ts |> Maybe.andThen .bestLabel |> Maybe.withDefault "-"

        balance =
            Address.getBalance adr |> Maybe.map .value |> Maybe.map (Locale.coinWithoutCode vc.locale (assetFromBase net)) |> Maybe.withDefault ""

        totalReceived =
            Address.getTotalReceived adr |> Maybe.map .value |> Maybe.map (Locale.coinWithoutCode vc.locale (assetFromBase net)) |> Maybe.withDefault ""

        currency =
            Address.getCurrency adr |> Maybe.map String.toUpper |> Maybe.withDefault ""

        cluster =
            adr |> Address.getClusterId |> Maybe.withDefault "-"

        key =
            Locale.string vc.locale
                >> text
                >> List.singleton
                >> div
                    [ css GraphComponents.tooltipProperty1DownLabel1Details.styles
                    ]

        val =
            List.singleton
                >> div
                    [ css GraphComponents.tooltipProperty1DownValue1Details.styles
                    ]
    in
    div
        [ css GraphComponents.tooltipProperty1DownDetails.styles
        ]
        [ div
            [ css GraphComponents.tooltipProperty1DownContent1Details.styles
            , css [ Css.whiteSpace Css.noWrap ]
            ]
            [ key "Category"
            , key "Label"
            , key "Balance"
            , key "Total Received"
            , key "Cluster"
            ]
        , div
            [ css GraphComponents.tooltipProperty1DownContent2Details.styles
            , css [ Css.whiteSpace Css.noWrap ]
            ]
            [ category
                |> text
                |> val
            , lbl
                |> text
                |> val
            , balance
                ++ " "
                ++ currency
                |> text
                |> val
            , totalReceived
                ++ " "
                ++ currency
                |> text
                |> val
            , cluster |> text |> val
            ]
        ]


utxoTx : View.Config -> Tx.UtxoTx -> Html msg
utxoTx vc tx =
    let
        key =
            Locale.string vc.locale
                >> text
                >> List.singleton
                >> div
                    [ css GraphComponents.tooltipProperty1DownLabel1Details.styles
                    ]

        val =
            List.singleton
                >> div
                    [ css GraphComponents.tooltipProperty1DownValue1Details.styles
                    ]
    in
    div
        [ css GraphComponents.tooltipProperty1DownDetails.styles
        ]
        [ div
            [ css GraphComponents.tooltipProperty1DownContent1Details.styles
            , css [ Css.whiteSpace Css.noWrap ]
            ]
            [ key "Tx hash"
            , key "Timestamp"
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
                |> Locale.timestampDateTimeUniform vc.locale vc.showTimeZoneOffset
                |> text
                |> val
            ]
        ]

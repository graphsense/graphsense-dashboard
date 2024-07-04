module View.Pathfinder.Tooltip exposing (view)

import Config.View as View
import Css
import Css.View as Css
import Hovercard
import Html.Attributes
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Model.Pathfinder.Tooltip exposing (Tooltip, TooltipType(..))
import Model.Pathfinder.Tx as Tx
import Theme.Html.GraphComponents as GraphComponents exposing (defaultProperty1DownAttributes)
import Util.Css as Css
import Util.View exposing (none, truncateLongIdentifierWithLengths)
import View.Locale as Locale


view : View.Config -> Tooltip -> Html msg
view vc tt =
    (case tt.type_ of
        UtxoTx t ->
            utxoTx vc t
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


utxoTx : View.Config -> Tx.UtxoTx -> Html msg
utxoTx vc tx =
    let
        key =
            Locale.string vc.locale
                >> text
                >> List.singleton
                >> div
                    [ css GraphComponents.property1DownLabel1Details.styles
                    ]

        val =
            List.singleton
                >> div
                    [ css GraphComponents.property1DownValue1Details.styles
                    ]
    in
    div
        [ css GraphComponents.property1DownDetails.styles
        ]
        [ div
            [ css GraphComponents.property1DownContent1Details.styles
            , css [ Css.whiteSpace Css.noWrap ]
            ]
            [ key "Tx hash"
            , key "Timestamp"
            ]
        , div
            [ css GraphComponents.property1DownContent2Details.styles
            , css [ Css.whiteSpace Css.noWrap ]
            ]
            [ tx.raw.txHash
                |> truncateLongIdentifierWithLengths 6 3
                |> text
                |> val
            , tx.raw.timestamp
                |> Locale.timestamp vc.locale
                |> text
                |> val
            ]
        ]

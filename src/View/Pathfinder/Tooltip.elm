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
        row key value =
            div
                [ css
                    [ Css.displayFlex
                    ]
                ]
                [ div
                    [ css
                        [ Css.lineHeight (Css.px 14.0625)
                        , Css.letterSpacing (Css.px -0.24)
                        , Css.fontSize (Css.px 12)
                        , Css.fontWeight (Css.int 400)
                        , Css.fontFamilies [ "Roboto" ]
                        , Css.color (Css.rgba 121 121 121 1)
                        , Css.whiteSpace Css.noWrap
                        , Css.width <| Css.px 100
                        ]
                    ]
                    [ text key
                    ]
                , div
                    [ css [ Css.whiteSpace Css.noWrap ]
                    ]
                    [ text value
                    ]
                ]
    in
    div
        [ css
            [ Css.padding2 (Css.px 10) (Css.px 15)
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.property "gap" "6px"
            ]
        ]
        [ tx.raw.txHash
            |> truncateLongIdentifierWithLengths 6 3
            |> row "Tx hash"
        ]

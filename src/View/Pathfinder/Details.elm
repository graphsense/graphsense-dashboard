module View.Pathfinder.Details exposing (DataTabConfig, closeAttrs, dataTab, valuesToCell)

import Api.Data
import Config.View as View
import Css
import Css.Pathfinder exposing (fullWidth)
import Html.Styled exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Model.Currency as Currency
import Msg.Pathfinder exposing (Msg(..))
import RecordSetter as Rs
import Svg.Styled
import Theme.Html.SidePanelComponents as SidePanelComponents
import Util.View exposing (pointer)
import View.Locale as Locale


valuesToCell : View.Config -> Currency.AssetIdentifier -> Api.Data.Values -> { firstRowText : String, secondRowText : String, secondRowVisible : Bool }
valuesToCell vc asset value =
    { firstRowText = Locale.currency (View.toCurrency vc) vc.locale [ ( asset, value ) ]
    , secondRowText = ""
    , secondRowVisible = False
    }


closeAttrs : List (Svg.Styled.Attribute Msg)
closeAttrs =
    [ css
        [ Css.cursor Css.pointer
        , Css.important <| Css.right <| Css.px 6
        , Css.important <| Css.top <| Css.px 0
        , Css.important <| Css.left <| Css.unset
        ]
    , onClick UserClosedDetailsView
    ]


type alias DataTabConfig msg =
    { title : Html msg
    , content : Maybe (Html msg)
    , onClick : msg
    }


dataTab : DataTabConfig msg -> Html msg
dataTab config =
    let
        attr =
            [ pointer
            , onClick config.onClick
            , css [ Css.zIndex <| Css.int 2 ]
            ]
    in
    config.content
        |> Maybe.map
            (\content ->
                SidePanelComponents.sidePanelDataTabOpenWithAttributes
                    (SidePanelComponents.sidePanelDataTabOpenAttributes
                        |> Rs.s_titleRow attr
                        |> Rs.s_root [ css fullWidth ]
                    )
                    { root =
                        { contentInstance =
                            Html.Styled.div
                                [ css fullWidth
                                ]
                                [ content
                                ]
                        , titleInstance = config.title
                        }
                    }
            )
        |> Maybe.withDefault
            (SidePanelComponents.sidePanelDataTabClosedWithAttributes
                (SidePanelComponents.sidePanelDataTabClosedAttributes
                    |> Rs.s_root (css fullWidth :: attr)
                )
                { root =
                    { titleInstance = config.title
                    }
                }
            )

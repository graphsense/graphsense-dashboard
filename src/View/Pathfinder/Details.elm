module View.Pathfinder.Details exposing (DataTabConfig, closeAttrs, dataTab, emptyCell, valuesToCell)

-- import Msg.Pathfinder exposing (Msg(..))

import Api.Data
import Config.View as View
import Css
import Css.Pathfinder exposing (fullWidth)
import Html.Styled exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Model.Currency as Currency
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


emptyCell : { firstRowText : String, secondRowText : String, secondRowVisible : Bool }
emptyCell =
    { firstRowText = ""
    , secondRowText = ""
    , secondRowVisible = False
    }


closeAttrs : msg -> List (Svg.Styled.Attribute msg)
closeAttrs closeMsg =
    [ css
        [ Css.cursor Css.pointer
        , Css.important <| Css.right <| Css.px 6
        , Css.important <| Css.top <| Css.px 0
        , Css.important <| Css.left <| Css.unset
        ]
    , onClick closeMsg
    ]


type alias DataTabConfig msg =
    { title : Html msg
    , disabled : Bool
    , content : Maybe (Html msg)
    , onClick : msg
    }


dataTab : DataTabConfig msg -> Html msg
dataTab config =
    let
        attr =
            [ pointer
            , css [ Css.zIndex <| Css.int 2 ]
            ]
                ++ (if not config.disabled then
                        [ onClick config.onClick ]

                    else
                        []
                   )

        dis =
            if config.disabled then
                [ Css.num 0.5 |> Css.opacity
                , Css.cursor Css.notAllowed |> Css.important
                ]
                    |> css
                    |> List.singleton

            else
                []
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
                    |> Rs.s_root (css fullWidth :: dis ++ attr)
                )
                { root =
                    { titleInstance = config.title
                    }
                }
            )

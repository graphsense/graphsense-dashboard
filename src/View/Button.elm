module View.Button exposing (BtnConfig, actorLink, button, buttonWithAttributes, defaultConfig, linkButtonBlue, linkButtonUnderlinedGray, primaryButton, primaryButtonGreen, secondaryButton, tool)

import Config.View as View
import Css
import Css.View
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import RecordSetter as Rs
import Route exposing (toUrl)
import Route.Graph as Route
import Theme.Colors
import Theme.Html.Buttons as Buttons
import Util.Css
import Util.View exposing (none, onClickWithStop, pointer)
import View.Locale as Locale


type alias BtnConfig msg =
    { icon : Maybe (Html msg)
    , text : String
    , onClick : Maybe msg
    , state : Buttons.ButtonState
    , style : Buttons.ButtonStyle
    , size : Buttons.ButtonSize
    , disabled : Bool
    , onClickWithStop : Bool
    }


defaultConfig : BtnConfig msg
defaultConfig =
    { icon = Nothing
    , text = ""
    , onClick = Nothing
    , state = Buttons.ButtonStateRegular
    , style = Buttons.ButtonStylePrimary
    , size = Buttons.ButtonSizeMedium
    , disabled = False
    , onClickWithStop = False
    }


tool :
    View.Config
    ->
        { icon : FontAwesome.Icon
        }
    -> List (Attribute msg)
    -> Html msg
tool vc { icon } attr =
    FontAwesome.icon icon
        |> Html.Styled.fromUnstyled
        |> List.singleton
        |> span
            ((Css.View.tool vc |> css)
                :: attr
            )


actorLink : View.Config -> String -> String -> Html msg
actorLink vc id label =
    a
        [ href
            (Route.actorRoute id Nothing
                |> Route.graphRoute
                |> toUrl
            )
        , Css.View.link vc |> css
        ]
        [ span []
            [ FontAwesome.icon FontAwesome.user
                |> Html.Styled.fromUnstyled
            ]
        , span
            [ css
                [ Css.paddingLeft (Css.rem 0.2) ]
            ]
            [ text label ]
        ]


button : View.Config -> BtnConfig msg -> Html msg
button =
    buttonWithAttributes { root = [], button = [] }


buttonWithAttributes : { root : List (Attribute msg), button : List (Attribute msg) } -> View.Config -> BtnConfig msg -> Html msg
buttonWithAttributes attr vc btn =
    let
        clickAttr =
            if btn.onClickWithStop then
                onClickWithStop

            else
                onClick

        state =
            if btn.disabled then
                Buttons.ButtonStateDisabled

            else
                btn.state

        style =
            ((Css.paddingTop <| Css.px 2)
                :: (if btn.disabled then
                        [ Util.Css.overrideBlack Theme.Colors.greyBlue100 ]

                    else
                        []
                   )
                |> css
            )
                :: (if state /= Buttons.ButtonStateDisabled then
                        pointer
                            :: (btn.onClick
                                    |> Maybe.map (clickAttr >> List.singleton)
                                    |> Maybe.withDefault []
                               )

                    else
                        []
                   )
                ++ attr.root
    in
    Buttons.buttonWithAttributes
        (Buttons.buttonAttributes
            |> Rs.s_root style
            |> Rs.s_button attr.button
        )
        { root =
            { state = state
            , type_ =
                if btn.icon == Nothing then
                    Buttons.ButtonTypeText

                else
                    Buttons.ButtonTypeTextIcon
            , style = btn.style
            , size = btn.size
            , buttonText = Locale.string vc.locale btn.text
            , iconInstance = btn.icon |> Maybe.withDefault none
            , iconVisible = btn.icon /= Nothing
            }
        }


primaryButton : View.Config -> BtnConfig msg -> Html msg
primaryButton vc btn =
    button vc { btn | style = Buttons.ButtonStylePrimary }


primaryButtonGreen : View.Config -> BtnConfig msg -> Html msg
primaryButtonGreen vc btn =
    buttonWithAttributes
        { root =
            [ Css.property "background-color" Theme.Colors.green300
                |> Css.important
            ]
                |> css
                |> List.singleton
        , button =
            [ Css.property "color" Theme.Colors.grey900
                |> Css.important
            ]
                |> css
                |> List.singleton
        }
        vc
        { btn
            | style = Buttons.ButtonStylePrimary
        }


secondaryButton : View.Config -> BtnConfig msg -> Html msg
secondaryButton vc btn =
    button vc { btn | style = Buttons.ButtonStyleOutlined }


linkButtonUnderlinedGray : View.Config -> BtnConfig msg -> Html msg
linkButtonUnderlinedGray vc btn =
    button vc
        { btn
            | style = Buttons.ButtonStyleTextGrey
        }


linkButtonBlue : View.Config -> BtnConfig msg -> Html msg
linkButtonBlue vc btn =
    button vc
        { btn
            | style = Buttons.ButtonStyleTextBlue
        }

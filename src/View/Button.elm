module View.Button exposing (BtnConfig, actorLink, btnDefaultConfig, linkButtonBlue, linkButtonUnderlinedGray, primaryButton, primaryButtonGreen, secondaryButton, tool)

import Config.View as View
import Css
import Css.View
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Maybe.Extra
import RecordSetter as Rs
import Route exposing (toUrl)
import Route.Graph as Route
import Theme.Colors
import Theme.Html.Buttons as Buttons
import Util.View exposing (none, onClickWithStop, pointer)
import View.Locale as Locale


type alias BtnConfig msg =
    { icon : Maybe (Html msg)
    , text : String
    , onClick : Maybe msg
    , disabled : Bool
    , onClickWithStop : Bool
    }


btnDefaultConfig : BtnConfig msg
btnDefaultConfig =
    { icon = Nothing, text = "", onClick = Nothing, disabled = False, onClickWithStop = False }


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


primaryButton : View.Config -> BtnConfig msg -> Html msg
primaryButton vc btn =
    primaryButtonWithAttributes vc
        btn
        { container = [], button = [] }


primaryButtonGreen : View.Config -> BtnConfig msg -> Html msg
primaryButtonGreen vc btn =
    primaryButtonWithAttributes vc
        btn
        { container =
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


primaryButtonWithAttributes : View.Config -> BtnConfig msg -> { container : List (Attribute msg), button : List (Attribute msg) } -> Html msg
primaryButtonWithAttributes vc btn attr =
    let
        clickAttr =
            if btn.onClickWithStop then
                onClickWithStop

            else
                onClick

        style =
            ([ Css.paddingTop <| Css.px 2
             ]
                |> css
            )
                :: (btn.onClick
                        |> Maybe.map (clickAttr >> List.singleton)
                        |> Maybe.withDefault []
                   )
                ++ attr.container
    in
    Buttons.buttonWithAttributes
        (Buttons.buttonAttributes
            |> Rs.s_root style
            |> Rs.s_button attr.button
        )
        { root =
            { state = Buttons.ButtonStateRegular
            , type_ =
                if btn.icon == Nothing then
                    Buttons.ButtonTypeText

                else
                    Buttons.ButtonTypeTextIcon
            , style = Buttons.ButtonStylePrimary
            , size = Buttons.ButtonSizeMedium
            , buttonText = Locale.string vc.locale btn.text
            , iconInstance = btn.icon |> Maybe.withDefault none
            , iconVisible = True
            }
        }


secondaryButton : View.Config -> BtnConfig msg -> Html msg
secondaryButton vc btn =
    let
        clickAttr =
            if btn.onClickWithStop then
                onClickWithStop

            else
                onClick

        style =
            ([ Css.paddingTop <| Css.px 2
             ]
                |> css
            )
                :: (btn.onClick
                        |> Maybe.map (clickAttr >> List.singleton)
                        |> Maybe.withDefault []
                   )
                ++ (if btn.disabled then
                        []

                    else
                        [ pointer ]
                   )
    in
    Buttons.buttonWithAttributes
        (Buttons.buttonAttributes
            |> Rs.s_root style
        )
        { root =
            { state = Buttons.ButtonStateRegular
            , type_ =
                if btn.icon == Nothing then
                    Buttons.ButtonTypeText

                else
                    Buttons.ButtonTypeTextIcon
            , style = Buttons.ButtonStyleOutlined
            , size = Buttons.ButtonSizeMedium
            , buttonText = Locale.string vc.locale btn.text
            , iconInstance = btn.icon |> Maybe.withDefault none
            , iconVisible = True
            }
        }


linkButtonUnderlinedGray : View.Config -> BtnConfig msg -> Html msg
linkButtonUnderlinedGray vc btn =
    let
        clickAttr =
            if btn.onClickWithStop then
                onClickWithStop

            else
                onClick

        style =
            ([ Css.cursor Css.pointer
             , Css.paddingTop <| Css.px 2
             ]
                |> css
            )
                :: (btn.onClick
                        |> Maybe.map (clickAttr >> List.singleton)
                        |> Maybe.withDefault []
                   )
    in
    Buttons.buttonWithAttributes
        (Buttons.buttonAttributes |> Rs.s_root style)
        { root =
            { state = Buttons.ButtonStateRegular
            , type_ = Buttons.ButtonTypeTextIcon
            , style = Buttons.ButtonStyleTextGrey
            , size = Buttons.ButtonSizeMedium
            , buttonText = Locale.string vc.locale btn.text
            , iconInstance = btn.icon |> Maybe.withDefault none
            , iconVisible = Maybe.Extra.isJust btn.icon
            }
        }


linkButtonBlue : View.Config -> BtnConfig msg -> Html msg
linkButtonBlue vc btn =
    let
        clickAttr =
            if btn.onClickWithStop then
                onClickWithStop

            else
                onClick

        style =
            ([ Css.cursor Css.pointer
             , Css.paddingTop <| Css.px 2
             ]
                |> css
            )
                :: (btn.onClick
                        |> Maybe.map (clickAttr >> List.singleton)
                        |> Maybe.withDefault []
                   )
    in
    Buttons.buttonWithAttributes
        (Buttons.buttonAttributes |> Rs.s_root style)
        { root =
            { state = Buttons.ButtonStateRegular
            , type_ = Buttons.ButtonTypeText
            , style = Buttons.ButtonStyleTextBlue
            , size = Buttons.ButtonSizeMedium
            , buttonText = Locale.string vc.locale btn.text
            , iconInstance = btn.icon |> Maybe.withDefault none
            , iconVisible = Maybe.Extra.isJust btn.icon
            }
        }

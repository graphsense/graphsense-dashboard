module View.Button exposing (BtnConfig, actorLink, btnDefaultConfig, linkButtonBlue, linkButtonUnderlinedGray, primaryButton, secondaryButton, tool)

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
import Theme.Html.Buttons as Btns
import Util.View exposing (none, onClickWithStop)
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
    case btn.icon of
        Just icon ->
            Btns.buttonTypeTextIconStateRegularStylePrimaryWithAttributes
                (Btns.buttonTypeTextIconStateRegularStylePrimaryAttributes
                    |> Rs.s_typeTextIconStateRegularStylePrimary
                        style
                )
                { typeTextIconStateRegularStylePrimary =
                    { buttonText = Locale.string vc.locale btn.text
                    , iconInstance = icon
                    , iconVisible = True
                    }
                }

        Nothing ->
            Btns.buttonTypeTextStateRegularStylePrimaryWithAttributes
                (Btns.buttonTypeTextStateRegularStylePrimaryAttributes
                    |> Rs.s_typeTextStateRegularStylePrimary
                        style
                )
                { typeTextStateRegularStylePrimary =
                    { buttonText = Locale.string vc.locale btn.text
                    , iconInstance = none
                    , iconVisible = False
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
    case btn.icon of
        Just icon ->
            Btns.buttonTypeTextIconStateRegularStyleOutlinedWithAttributes
                (Btns.buttonTypeTextIconStateRegularStyleOutlinedAttributes
                    |> Rs.s_typeTextIconStateRegularStyleOutlined
                        style
                )
                { typeTextIconStateRegularStyleOutlined =
                    { buttonText = Locale.string vc.locale btn.text
                    , iconInstance = icon
                    , iconVisible = True
                    }
                }

        Nothing ->
            Btns.buttonTypeTextStateRegularStyleOutlinedWithAttributes
                (Btns.buttonTypeTextStateRegularStyleOutlinedAttributes
                    |> Rs.s_typeTextStateRegularStyleOutlined
                        style
                )
                { typeTextStateRegularStyleOutlined =
                    { buttonText = Locale.string vc.locale btn.text
                    , iconInstance = none
                    , iconVisible = False
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
    Btns.buttonTypeTextIconStateRegularStyleTextGreWithAttributes
        (Btns.buttonTypeTextIconStateRegularStyleTextGreAttributes
            |> Rs.s_typeTextIconStateRegularStyleTextGre style
        )
        { typeTextIconStateRegularStyleTextGre =
            { buttonText = Locale.string vc.locale btn.text
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
    Btns.buttonTypeTextStateRegularStyleTextBlueWithAttributes
        (Btns.buttonTypeTextStateRegularStyleTextBlueAttributes
            |> Rs.s_typeTextStateRegularStyleTextBlue
                style
        )
        { typeTextStateRegularStyleTextBlue =
            { buttonText = Locale.string vc.locale btn.text
            , iconInstance = btn.icon |> Maybe.withDefault none
            , iconVisible = Maybe.Extra.isJust btn.icon
            }
        }

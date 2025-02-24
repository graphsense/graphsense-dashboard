module View.Button exposing (BtnConfig, actorLink, primaryButton, secondaryButton, tool)

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
import Theme.Html.Buttons as Btns
import Util.View exposing (none)
import View.Locale as Locale


type alias BtnConfig msg =
    { icon : Maybe (Html msg)
    , text : String
    , onClick : msg
    , disabled : Bool
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


primaryButton : View.Config -> BtnConfig msg -> Html msg
primaryButton vc btn =
    let
        style =
            [ onClick btn.onClick
            , [ Css.cursor Css.pointer
              , Css.paddingTop <| Css.px 2
              ]
                |> css
            ]
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
        style =
            [ onClick btn.onClick
            , [ Css.cursor Css.pointer
              , Css.paddingTop <| Css.px 2
              ]
                |> css
            ]
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

module User.View exposing (hovercard, user)

import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events as Events exposing (onInput)
import Locale.Model as Locale
import Locale.View as Locale
import Modal.View as Modal
import Model exposing (Auth(..), Msg(..), RequestLimit(..), UserModel)
import Time
import User.Css as Css
import Util.View exposing (nona, none)
import View.Button as Button
import View.Config exposing (Config)
import View.Css as Css


user : Config -> UserModel -> Html Msg
user vc model =
    div
        [ id "user"
        , Css.root vc |> css
        ]
        [ Button.tool vc
            { icon = FontAwesome.user
            }
            [ id "userTool"
            , Events.onMouseOver (UserHoversUserIcon "userTool")
            , Events.onClick (UserHoversUserIcon "userTool")
            ]
        ]


hovercard : Config -> UserModel -> List (Html Msg)
hovercard vc model =
    (case model.auth of
        Authorized auth ->
            [ auth.requestLimit |> requestLimit vc
            , auth.expiration |> Maybe.map (expiration vc) |> Maybe.withDefault none
            ]

        Unknown ->
            []

        _ ->
            [ apiKeyForm vc model
            ]
    )
        ++ [ Modal.part vc
                (Locale.string vc.locale "language")
                [ localeSwitch vc ]
           ]
        |> div
            [ Events.onMouseLeave UserLeftUserHovercard
            , Css.hovercardRoot vc |> css
            ]
        |> List.singleton


requestLimit : Config -> RequestLimit -> Html Msg
requestLimit vc rl =
    Modal.part vc
        (Locale.string vc.locale "Request limit")
        [ div
            [ Css.requestLimitRoot vc |> css
            ]
            (case rl of
                Unlimited ->
                    [ div
                        [ Css.requestLimit vc |> css
                        ]
                        [ Locale.string vc.locale "unlimited" |> text
                        ]
                    ]

                Limited { remaining, limit, reset } ->
                    [ div
                        [ Css.requestLimit vc |> css
                        ]
                        [ Locale.interpolated vc.locale "{0}/{1}" [ String.fromInt remaining, String.fromInt limit ] |> text
                        ]
                    , if remaining > 20 then
                        none

                      else
                        div
                            [ Css.requestReset vc |> css
                            ]
                            [ reset
                                |> String.fromInt
                                |> List.singleton
                                |> Locale.interpolated vc.locale "reset in {0}s"
                                |> text
                            ]
                    ]
            )
        ]


expiration : Config -> Time.Posix -> Html Msg
expiration vc time =
    Time.posixToMillis time
        |> Locale.timestamp vc.locale
        |> text


localeSwitch : Config -> Html Msg
localeSwitch vc =
    Locale.locales
        |> List.map
            (\( locale, language ) ->
                option
                    [ value locale
                    , Debug.log "locale" locale
                        == Debug.log "vc.locale" vc.locale.locale
                        |> selected
                    ]
                    [ Locale.string vc.locale language |> text
                    ]
            )
        |> select
            [ onInput UserSwitchesLocale
            , Css.input vc |> css
            ]


apiKeyForm : Config -> UserModel -> Html Msg
apiKeyForm vc model =
    Modal.part vc
        (Locale.string vc.locale "API key")
        [ Html.Styled.form
            [ Events.onSubmit UserSubmitsApiKeyForm
            ]
            [ input
                [ Css.input vc |> css
                , Events.onBlur UserSubmitsApiKeyForm
                , Events.onInput UserInputsApiKeyForm
                , disabled <| model.auth == Loading
                , value model.apiKey
                ]
                []
            ]
        ]

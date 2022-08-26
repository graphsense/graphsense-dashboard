module View.User exposing (apiKeyForm, hovercard, user)

import Config.View exposing (Config)
import Css.User as Css
import Css.View as Css
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events as Events exposing (onClick, onInput, stopPropagationOn)
import Json.Decode
import Model exposing (Auth(..), Msg(..), RequestLimit(..), UserModel)
import Model.Locale as Locale
import Time
import Util.View exposing (loadingSpinner, nona, none)
import View.Button as Button
import View.Dialog as Dialog
import View.Locale as Locale


user : Config -> UserModel -> Html Msg
user vc model =
    div
        [ Css.root vc |> css
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

        Unauthorized loading _ ->
            [ apiKeyForm vc loading model
            ]
    )
        ++ [ Dialog.part vc
                "Language"
                [ localeSwitch vc ]
           , Dialog.part vc
                "Display"
                [ input
                    [ type_ "checkbox"
                    , checked vc.lightmode
                    , onClick UserClickedLightmode
                    ]
                    []
                , Locale.string vc.locale "Light mode"
                    |> text
                ]
           ]
        ++ (case model.auth of
                Authorized auth ->
                    [ if auth.loggingOut then
                        loadingSpinner vc Css.loadingSpinner

                      else
                        button
                            [ Css.logoutButton vc |> css
                            , onClick UserClickedLogout
                            ]
                            [ Locale.string vc.locale "Logout" |> text
                            ]
                    ]

                _ ->
                    []
           )
        |> div
            [ Events.on "mouseleave"
                (Json.Decode.oneOf
                    [ Json.Decode.at [ "relatedTarget" ] (Json.Decode.null False)
                        |> Json.Decode.map (\_ -> NoOp)
                    , Json.Decode.succeed UserLeftUserHovercard
                    ]
                )
            , Events.stopPropagationOn "click" (Json.Decode.succeed ( NoOp, True ))
            , Css.hovercardRoot vc |> css
            ]
        |> List.singleton


requestLimit : Config -> RequestLimit -> Html Msg
requestLimit vc rl =
    Dialog.part vc
        "Request limit"
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
                    , if reset == 0 || remaining > Model.showResetCounterAtRemaining then
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
                    , locale
                        == vc.locale.locale
                        |> selected
                    ]
                    [ Locale.string vc.locale language |> text
                    ]
            )
        |> select
            [ onInput UserSwitchesLocale
            , Css.input vc |> css
            ]


apiKeyForm : Config -> Bool -> UserModel -> Html Msg
apiKeyForm vc loading model =
    Dialog.part vc
        "Please provide an API key"
        [ Html.Styled.form
            [ Events.onSubmit UserSubmitsApiKeyForm
            ]
            [ input
                [ Css.input vc |> css
                , Events.onBlur UserSubmitsApiKeyForm
                , Events.onInput UserInputsApiKeyForm
                , disabled loading
                , value model.apiKey
                ]
                []
            , if loading then
                loadingSpinner vc Css.loadingSpinner

              else
                input
                    [ Css.primary vc |> css
                    , type_ "submit"
                    , Locale.string vc.locale "OK" |> value
                    , disabled loading
                    ]
                    []
            ]
        ]

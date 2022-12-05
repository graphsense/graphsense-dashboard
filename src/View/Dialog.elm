module View.Dialog exposing (..)

import Config.View exposing (Config)
import Css.Dialog as Css
import Css.View
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model exposing (Msg(..))
import Model.Dialog exposing (..)
import Util.View
import View.Locale as Locale


view : Config -> Model Msg -> Html Msg
view vc model =
    div
        [ Css.dialog vc |> css
        ]
        [ case model of
            Confirm conf ->
                confirm vc conf

            Options conf ->
                options_ vc conf
        ]


confirm : Config -> ConfirmConfig Msg -> Html Msg
confirm vc { message, onYes, onNo } =
    part vc
        message
        [ div
            [ Css.buttons vc |> css
            ]
            [ button
                [ Css.View.primary vc |> css
                , UserClickedConfirm onYes |> onClick
                ]
                [ Locale.string vc.locale "Yes" |> text
                ]
            , button
                [ Css.View.primary vc |> css
                , UserClickedConfirm onNo |> onClick
                ]
                [ Locale.string vc.locale "No" |> text
                ]
            ]
        ]


options_ : Config -> OptionsConfig Msg -> Html Msg
options_ vc { message, options } =
    part vc
        message
        [ options
            |> List.map
                (\( title, msg ) ->
                    button
                        [ Css.View.primary vc |> css
                        , onClick <| UserClickedOption msg
                        ]
                        [ Locale.string vc.locale title |> text
                        ]
                )
            |> div
                []
        ]


part : Config -> String -> List (Html msg) -> Html msg
part vc title content =
    div
        [ Css.part vc |> css
        ]
        (h4
            [ Css.heading vc |> css
            ]
            [ Locale.string vc.locale title
                |> text
            ]
            :: content
        )


headRow : Config -> String -> Maybe msg -> Html msg
headRow vc title onClose =
    div
        [ Css.headRow vc |> css
        ]
        [ title
            |> Locale.string vc.locale
            |> text
            |> List.singleton
            |> span
                [ Css.headRowText vc |> css
                ]
        , onClose
            |> Maybe.map
                (\click ->
                    button
                        [ Css.headRowClose vc |> css
                        , onClick click
                        ]
                        [ FontAwesome.icon FontAwesome.times
                            |> Html.Styled.fromUnstyled
                        ]
                )
            |> Maybe.withDefault Util.View.none
        ]


body : Config -> { onSubmit : msg } -> List (Html msg) -> Html msg
body vc { onSubmit } =
    Html.Styled.form
        [ Css.body vc |> css
        , Html.Styled.Events.onSubmit onSubmit
        ]

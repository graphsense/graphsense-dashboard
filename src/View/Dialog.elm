module View.Dialog exposing (..)

import Config.View exposing (Config)
import Css.Dialog as Css
import Css.View
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Dialog exposing (..)
import Util.View
import View.Locale as Locale


view : Config -> Model msg -> Html msg
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


confirm : Config -> ConfirmConfig msg -> Html msg
confirm vc { message, onYes, onNo } =
    part vc
        message
        [ div
            [ Css.buttons vc |> css
            ]
            [ button
                [ Css.View.primary vc |> css
                , onClick onYes
                ]
                [ Locale.string vc.locale "Yes" |> text
                ]
            , button
                [ Css.View.primary vc |> css
                , onClick onNo
                ]
                [ Locale.string vc.locale "No" |> text
                ]
            ]
        ]


options_ : Config -> OptionsConfig msg -> Html msg
options_ vc { message, options } =
    part vc
        message
        [ options
            |> List.map
                (\( title, msg ) ->
                    button
                        [ Css.View.primary vc |> css
                        , onClick msg
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


body : Config -> List (Html msg) -> Html msg
body vc =
    div
        [ Css.body vc |> css
        ]

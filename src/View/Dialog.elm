module View.Dialog exposing (..)

import Config.View exposing (Config)
import Css.Button
import Css.Dialog as Css
import Css.View
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model exposing (Msg(..))
import Model.Dialog exposing (..)
import Util.View exposing (addDot)
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

            Error conf ->
                error vc conf

            Info conf ->
                info vc conf
        ]


confirm : Config -> ConfirmConfig Msg -> Html Msg
confirm vc { message, onYes, onNo } =
    part vc
        message
        [ div
            [ Css.buttons vc |> css
            ]
            [ button
                [ Css.Button.primary vc |> css
                , UserClickedConfirm onYes |> onClick
                ]
                [ Locale.string vc.locale "Yes" |> text
                ]
            , button
                [ Css.Button.primary vc |> css
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
                        [ Css.Button.primary vc |> css
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


error : Config -> ErrorConfig msg -> Html msg
error vc err =
    let
        title =
            case err.type_ of
                AddressNotFound addrs ->
                    if List.length addrs > 1 then
                        "Addresses not found"

                    else
                        "Address not found"

        take =
            3

        details =
            case err.type_ of
                AddressNotFound addrs ->
                    [ addrs
                        |> List.take take
                        |> List.map (text >> List.singleton >> li [ Css.View.listItem vc |> css ])
                        |> (\lis ->
                                if List.length addrs > take then
                                    (List.length addrs - take)
                                        |> String.fromInt
                                        |> List.singleton
                                        |> Locale.interpolated vc.locale "... and {0} more"
                                        |> text
                                        |> List.singleton
                                        |> li []
                                        |> List.singleton
                                        |> (++) lis

                                else
                                    lis
                           )
                        |> ul []
                        |> List.singleton
                        |> Util.View.p vc []
                    , div
                        []
                        [ Locale.string vc.locale "There could be various reasons"
                            |> (\s -> s ++ ":")
                            |> text
                            |> List.singleton
                            |> Util.View.p vc []
                        , ul
                            []
                            [ li [ Css.View.listItem vc |> css ]
                                [ (if List.length addrs > 1 then
                                    "There are no transactions associated with these addresses and they are therefore not found on the blockchain."

                                   else
                                    "There are no transactions associated with this address and it is therefore not found on the blockchain."
                                  )
                                    |> Locale.string vc.locale
                                    |> addDot
                                    |> text
                                ]
                            , li
                                [ Css.View.listItem vc |> css ]
                                [ (if List.length addrs > 1 then
                                    "They are possibly not yet in our database"

                                   else
                                    "It is possibly not yet in our database"
                                  )
                                    |> Locale.string vc.locale
                                    |> addDot
                                    |> text
                                ]
                            , li [ Css.View.listItem vc |> css ]
                                [ Locale.string vc.locale "There are typos"
                                    |> addDot
                                    |> text
                                ]
                            ]
                            |> List.singleton
                            |> Util.View.p vc []
                        ]
                    ]
    in
    part vc title <|
        details
            ++ [ button
                    [ Css.Button.primary vc |> css
                    , onClick err.onOk
                    ]
                    [ Locale.string vc.locale "OK" |> text
                    ]
               ]


info : Config -> InfoConfig Msg -> Html Msg
info vc inf =
    part vc
        (Locale.interpolated vc.locale inf.info inf.variables)
        [ div
            [ Css.singleButton vc |> css
            ]
            [ button
                [ Css.Button.primary vc |> css
                , UserClickedConfirm inf.onOk |> onClick
                ]
                [ Locale.string vc.locale "OK" |> text
                ]
            ]
        ]

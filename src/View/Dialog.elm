module View.Dialog exposing (..)

import Config.View exposing (Config)
import Css.Dialog as Css
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Util.View
import View.Locale as Locale


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

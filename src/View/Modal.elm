module View.Modal exposing (part)

import Config.View exposing (Config)
import Css.Modal as Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)


part : Config -> String -> List (Html msg) -> Html msg
part vc title content =
    div
        [ Css.part vc |> css
        ]
        (h4
            [ Css.heading vc |> css
            ]
            [ text title
            ]
            :: content
        )

module Modal.View exposing (part)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Modal.Css as Css
import View.Config exposing (Config)


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

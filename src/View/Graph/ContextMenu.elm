module View.Graph.ContextMenu exposing (option, optionHtml, view)

import Config.View as View
import Css exposing (left, px, top)
import Css.ContextMenu
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Graph.ContextMenu exposing (..)
import Model.Graph.Coords exposing (Coords)
import Msg.Graph exposing (..)


view : View.Config -> Coords -> List (Html Msg) -> Html Msg
view vc coords =
    div
        [ top (px coords.y)
            :: left (px coords.x)
            :: Css.ContextMenu.root vc
            |> css
        , onClick UserClickedContextMenu
        , onMouseLeave UserLeftContextMenu
        ]


option : View.Config -> String -> msg -> Html msg
option vc title msg =
    optionHtml vc
        [ text title
        ]
        msg


optionHtml : View.Config -> List (Html msg) -> msg -> Html msg
optionHtml vc title msg =
    div
        [ Css.ContextMenu.option vc |> css
        , onClick msg
        ]
        title

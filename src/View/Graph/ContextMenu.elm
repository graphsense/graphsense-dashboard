module View.Graph.ContextMenu exposing (option, optionHtml, optionWithIcon, view)

import Config.View as View
import Css exposing (int, left, px, top, zIndex)
import Css.ContextMenu
import FontAwesome exposing (Icon)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Graph.ContextMenu exposing (..)
import Model.Graph.Coords exposing (Coords)
import Msg.Graph exposing (..)


view : View.Config -> Coords -> List (Html Msg) -> Html Msg
view vc coords =
    div
        [ top (px (coords.y - 5))
            :: left (px (coords.x - 5))
            :: zIndex (int 100)
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


optionWithIcon : View.Config -> String -> Icon -> msg -> Html msg
optionWithIcon vc title icon msg =
    optionHtml vc
        [ FontAwesome.icon icon
            |> Html.Styled.fromUnstyled
            |> List.singleton
            |> span
                [ Html.Styled.Attributes.css [ Css.marginRight <| Css.em 0.5 ] ]
        , title |> Html.Styled.text
        ]
        msg

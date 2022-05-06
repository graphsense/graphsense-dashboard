module Graph.View.Tool exposing (tool)

import Graph.Css as Css
import Graph.Model.Tool exposing (Tool)
import Graph.Msg exposing (Msg)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import View.Config exposing (Config)


tool : Config -> Tool -> Html Msg
tool vc t =
    button
        [ Css.tool vc |> css
        , title t.title
        , onClick t.msg
        ]
        [ t.icon
            |> Html.Styled.fromUnstyled
        ]

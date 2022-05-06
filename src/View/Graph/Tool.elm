module View.Graph.Tool exposing (tool)

import Config.View exposing (Config)
import Css.Graph as Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Graph.Tool exposing (Tool)
import Msg.Graph exposing (Msg)


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

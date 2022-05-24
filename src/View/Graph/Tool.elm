module View.Graph.Tool exposing (tool)

import Config.View exposing (Config)
import Css exposing (color)
import Css.Graph as Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Graph.Tool exposing (Tool)
import Util.View exposing (toCssColor)


tool : Config -> Tool msg -> Html msg
tool vc t =
    button
        [ Css.tool vc
            ++ (t.color |> Maybe.map (toCssColor >> color >> List.singleton) |> Maybe.withDefault [])
            |> css
        , title t.title
        , onClick t.msg
        ]
        [ t.icon
            |> Html.Styled.fromUnstyled
        ]

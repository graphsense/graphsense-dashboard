module View.Graph.Tool exposing (tool)

import Config.View exposing (Config)
import Css exposing (color)
import Css.Graph as Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Graph.Tool exposing (Tool)
import Msg.Graph exposing (Msg(..))
import Util.View exposing (toCssColor)
import View.Locale as Locale


tool : Config -> Tool msg -> Html msg
tool vc t =
    button
        [ Css.tool vc
            ++ (t.color |> Maybe.map (toCssColor >> color >> List.singleton) |> Maybe.withDefault [])
            |> css
        , Locale.string vc.locale t.title |> title
        , onClick t.msg
        , id t.title
        ]
        [ t.icon
            |> Html.Styled.fromUnstyled
        ]

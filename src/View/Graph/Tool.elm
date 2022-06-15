module View.Graph.Tool exposing (..)

import Browser.Dom as Dom
import Config.View exposing (Config)
import Css exposing (color)
import Css.Graph as Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Graph exposing (ActiveTool)
import Model.Graph.Tool as Tool exposing (Tool)
import Msg.Graph exposing (Msg(..))
import Tuple exposing (..)
import Util.View exposing (none, toCssColor)
import View.Graph.Configuration as Configuration
import View.Graph.Export as Export
import View.Graph.Legend as Legend
import View.Locale as Locale


tool : Config -> Tool msg -> Html msg
tool vc t =
    button
        [ Css.tool vc t.status
            ++ (t.color |> Maybe.map (toCssColor >> color >> List.singleton) |> Maybe.withDefault [])
            |> css
        , Locale.string vc.locale t.title |> title
        , onClick (t.msg t.title)
        , id t.title
        ]
        [ t.icon
            |> Html.Styled.fromUnstyled
        ]


toolbox : Config -> ActiveTool -> Html Msg
toolbox vc activeTool =
    div
        [ [ activeTool.element
                |> Maybe.map
                    (\( el, visible ) ->
                        el.element.x
                            + el.element.width
                            |> Css.px
                            |> Css.left
                    )
                |> Maybe.withDefault (Css.right (Css.px 0))
          , Css.position Css.absolute
          ]
            |> css
        ]
        [ div
            [ [ Css.right (Css.px 60) -- TODO matches the sidebar width hardcoded
              ]
                ++ (activeTool.element
                        |> Maybe.map second
                        |> Maybe.withDefault False
                        |> Css.toolbox vc
                   )
                |> css
            ]
            (case activeTool.toolbox of
                Tool.Legend data ->
                    Legend.legend vc data

                Tool.Configuration config ->
                    Configuration.configuration vc config

                Tool.Export ->
                    Export.export vc

                Tool.Import ->
                    Export.import_ vc
            )
        ]

module View.Graph.Tool exposing (..)

import Browser.Dom as Dom
import Config.View exposing (Config)
import Css exposing (color)
import Css.Graph as Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Graph exposing (ActiveTool, Model)
import Model.Graph.Tool as Tool exposing (Tool)
import Msg.Graph exposing (Msg(..))
import Tuple exposing (..)
import Util.View exposing (none, toCssColor)
import View.Graph.Configuration as Configuration
import View.Graph.ExportImport as Export
import View.Graph.Highlighter as Highlighter
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


toolbox : Config -> Model -> Html Msg
toolbox vc model =
    let
        isRight =
            case model.activeTool.toolbox of
                Tool.Legend data ->
                    True

                Tool.Configuration config ->
                    True

                Tool.Export ->
                    False

                Tool.Import ->
                    False

                Tool.Highlighter ->
                    True
    in
    div
        [ [ model.activeTool.element
                |> Maybe.map
                    (\( el, visible ) ->
                        if isRight then
                            Css.right (Css.px 0)

                        else
                            el.element.x
                                - (Maybe.map .x model.size |> Maybe.withDefault 80)
                                |> Css.px
                                |> Css.left
                    )
                |> Maybe.withDefault (Css.right (Css.px 0))
          , Css.position Css.absolute
          ]
            |> css
        ]
        [ div
            [ [ if isRight then
                    Css.right (Css.px 0)

                else
                    Css.left (Css.px 0)
              ]
                ++ (model.activeTool.element
                        |> Maybe.map second
                        |> Maybe.withDefault False
                        |> Css.toolbox vc
                   )
                |> css
            ]
            (case model.activeTool.toolbox of
                Tool.Legend data ->
                    Legend.legend vc data

                Tool.Configuration config ->
                    Configuration.configuration vc model.config

                Tool.Export ->
                    Export.export vc

                Tool.Import ->
                    Export.import_ vc

                Tool.Highlighter ->
                    Highlighter.tool vc model.highlights
            )
        ]
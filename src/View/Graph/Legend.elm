module View.Graph.Legend exposing (legend)

import Config.View as View
import Css
import Css.Graph as Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model.Graph.Legend exposing (..)
import Msg.Graph exposing (Msg)
import Util.View exposing (toCssColor)
import View.Locale as Locale


legend : View.Config -> List Item -> List (Html Msg)
legend vc items =
    if List.isEmpty items then
        div
            [ Css.legendItem vc |> css
            ]
            [ Locale.string vc.locale "Legend empty"
                |> text
            ]
            |> List.singleton

    else
        items
            |> List.map
                (\item ->
                    span
                        [ Css.legendItem vc |> css
                        ]
                        [ span
                            [ (item.color
                                |> toCssColor
                                |> Css.color
                              )
                                :: Css.legendItemColor vc
                                |> css
                            ]
                            [ text "◼" ]
                        , span
                            [ [ Css.legendItemTitle vc |> Css.batch

                              {- , Css.View.link vc |> Css.batch -}
                              ]
                                |> css

                            {- , href item.uri -}
                            {- , target "_blank" -}
                            {- , Json.Decode.succeed ( NoOp, True )
                               |> stopPropagationOn "click"
                            -}
                            ]
                            [ text item.title
                            ]
                        ]
                )

module View.Pathfinder.Table.Columns exposing (timestampDateMultiRowColumn, txColumn)

import Config.View as View
import Css
import Css.Pathfinder as PCSS
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Table
import Util.View exposing (loadingSpinner, longIdentifier, none)
import View.Locale as Locale


timestampDateMultiRowColumn : View.Config -> String -> (data -> Int) -> Table.Column data msg
timestampDateMultiRowColumn vc name accessor =
    Table.veryCustomColumn
        { name = name
        , viewData =
            \data ->
                Table.HtmlDetails []
                    [ let
                        d =
                            accessor data

                        date =
                            Locale.timestampDateUniform vc.locale d

                        time =
                            Locale.timestampTimeUniform vc.locale d
                      in
                      div [ [ PCSS.mGap |> Css.padding ] |> css ]
                        [ div [ [ PCSS.mGap |> Css.padding ] |> css ] [ text date ]
                        , div [ [ PCSS.mGap |> Css.padding, PCSS.sText |> Css.fontSize ] |> css ] [ text time ]
                        ]
                    ]
        , sorter = Table.increasingOrDecreasingBy accessor
        }


txColumn : View.Config -> String -> (data -> String) -> Table.Column data msg
txColumn vc name accessor =
    Table.veryCustomColumn
        { name = name
        , viewData =
            \data ->
                accessor data
                    |> longIdentifier vc
                    |> List.singleton
                    |> Table.HtmlDetails [ [ PCSS.mGap |> Css.padding ] |> css ]
        , sorter = Table.increasingOrDecreasingBy accessor
        }

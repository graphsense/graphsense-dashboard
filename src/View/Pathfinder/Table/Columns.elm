module View.Pathfinder.Table.Columns exposing (addressColumn, checkboxColumn, debitCreditColumn, timestampDateMultiRowColumn, txColumn, valueColumn)

import Api.Data
import Config.View as View
import Css
import Css.Pathfinder as PCSS
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Currency exposing (AssetIdentifier)
import Table
import Util.View exposing (copyableLongIdentifier, none)
import View.Locale as Locale
import View.Pathfinder.Icons exposing (inIcon, outIcon)


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

        -- , sorter = Table.increasingOrDecreasingBy accessor
        , sorter = Table.unsortable
        }


addressColumn : View.Config -> String -> (data -> String) -> Table.Column data msg
addressColumn =
    txColumn


txColumn : View.Config -> String -> (data -> String) -> Table.Column data msg
txColumn vc name accessor =
    Table.veryCustomColumn
        { name = name
        , viewData =
            \data ->
                accessor data
                    |> copyableLongIdentifier vc []
                    |> List.singleton
                    |> Table.HtmlDetails [ [ PCSS.mGap |> Css.padding ] |> css ]

        -- , sorter = Table.increasingOrDecreasingBy accessor
        , sorter = Table.unsortable
        }


checkboxColumn : View.Config -> String -> (data -> Bool) -> (data -> msg) -> Table.Column data msg
checkboxColumn _ name isChecked clickMsg =
    Table.veryCustomColumn
        { name = name
        , viewData =
            \data ->
                Table.HtmlDetails [ [ PCSS.mGap |> Css.padding ] |> css ]
                    [ input [ type_ "checkbox", onClick (clickMsg data), checked (isChecked data) ] []
                    ]
        , sorter = Table.unsortable
        }


debitCreditColumn : View.Config -> (data -> AssetIdentifier) -> String -> (data -> Api.Data.Values) -> Table.Column data msg
debitCreditColumn =
    valueColumnWithOptions False False


valueColumn : View.Config -> (data -> AssetIdentifier) -> String -> (data -> Api.Data.Values) -> Table.Column data msg
valueColumn =
    valueColumnWithOptions True True


valueColumnWithOptions : Bool -> Bool -> View.Config -> (data -> AssetIdentifier) -> String -> (data -> Api.Data.Values) -> Table.Column data msg
valueColumnWithOptions hideCode hideFlowIndicator vc getCoinCode name getValues =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> getValues data |> valuesCell vc hideCode hideFlowIndicator (getCoinCode data)

        -- , sorter = Table.decreasingOrIncreasingBy (\data -> getValues data |> valuesSorter vc (getCoinCode data))
        , sorter = Table.unsortable
        }


valuesCell : View.Config -> Bool -> Bool -> AssetIdentifier -> Api.Data.Values -> Table.HtmlDetails msg
valuesCell vc hideCode hideFlowIndicator coinCode values =
    let
        value =
            (if hideCode then
                Locale.currencyWithoutCode

             else
                Locale.currency
            )
                vc.locale
                [ ( coinCode, values ) ]

        flowIndicator =
            if String.startsWith "-" value then
                outIcon

            else
                inIcon
    in
    Table.HtmlDetails [ [ PCSS.mGap |> Css.padding ] |> css ]
        [ if hideFlowIndicator then
            none

          else
            flowIndicator
        , text value
        ]

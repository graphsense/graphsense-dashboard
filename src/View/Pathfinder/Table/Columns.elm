module View.Pathfinder.Table.Columns exposing (addressColumn, checkboxColumn, debitCreditColumn, stringColumn, timestampDateMultiRowColumn, txColumn, valueColumn, valueColumnWithOptions)

import Api.Data
import Config.View as View
import Css
import Css.Pathfinder as PCSS exposing (toAttr)
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Currency exposing (AssetIdentifier)
import Table
import Util.View exposing (copyableLongIdentifierPathfinder, none)
import View.Graph.Table exposing (valuesSorter)
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


type alias ColumnConfig data msg =
    { label : String
    , accessor : data -> String
    , onClick : Maybe (data -> msg)
    }


addressColumn : View.Config -> ColumnConfig data msg -> Maybe (data -> Maybe String) -> Table.Column data msg
addressColumn vc cc lblfn =
    identifierColumn (lblfn |> Maybe.withDefault (\_ -> Nothing)) vc cc


identifierColumn : (data -> Maybe String) -> View.Config -> ColumnConfig data msg -> Table.Column data msg
identifierColumn lblfn vc { label, accessor, onClick } =
    let
        tagcss =
            [ Css.width (Css.px 15), Css.display Css.inlineBlock ] |> toAttr
    in
    Table.veryCustomColumn
        { name = label
        , viewData =
            \data ->
                (case lblfn data of
                    Just lbl ->
                        [ span [ tagcss, title lbl ] [ FontAwesome.icon FontAwesome.tag |> Html.Styled.fromUnstyled ] ]

                    _ ->
                        [ span [ tagcss ] [] ]
                )
                    ++ (accessor data |> copyableLongIdentifierPathfinder vc [] |> List.singleton)
                    |> Table.HtmlDetails
                        (([ PCSS.mGap |> Css.padding, Css.textAlign Css.right ] |> css)
                            :: (onClick
                                    |> Maybe.map
                                        (\cl ->
                                            [ cl data |> Html.Styled.Events.onClick
                                            , css [ Css.cursor Css.pointer ]
                                            ]
                                        )
                                    |> Maybe.withDefault []
                               )
                        )
        , sorter = Table.increasingOrDecreasingBy accessor
        }


txColumn : View.Config -> ColumnConfig data msg -> Table.Column data msg
txColumn =
    identifierColumn (\_ -> Nothing)


stringColumn : View.Config -> ColumnConfig data msg -> Table.Column data msg
stringColumn vc { label, accessor } =
    Table.veryCustomColumn
        { name = label
        , viewData =
            \data ->
                Table.HtmlDetails [ [ PCSS.mGap |> Css.padding ] |> css ]
                    [ text (accessor data)
                    ]
        , sorter = Table.unsortable
        }


type alias CheckboxColumnConfig data msg =
    { isChecked : data -> Bool
    , onClick : data -> msg
    }


checkboxColumn : View.Config -> CheckboxColumnConfig data msg -> Table.Column data msg
checkboxColumn _ { isChecked, onClick } =
    Table.veryCustomColumn
        { name = ""
        , viewData =
            \data ->
                Table.HtmlDetails [ [ PCSS.mGap |> Css.padding ] |> css ]
                    [ input
                        [ type_ "checkbox"
                        , onClick data |> Html.Styled.Events.onClick
                        , isChecked data |> checked
                        ]
                        []
                    ]
        , sorter = Table.unsortable
        }


debitCreditColumn : View.Config -> (data -> AssetIdentifier) -> String -> (data -> Api.Data.Values) -> Table.Column data msg
debitCreditColumn =
    valueColumnWithOptions
        { sortable = True
        , hideCode = False
        , hideFlowIndicator = False
        }


valueColumn : View.Config -> (data -> AssetIdentifier) -> String -> (data -> Api.Data.Values) -> Table.Column data msg
valueColumn =
    valueColumnWithOptions
        { sortable = True
        , hideCode = True
        , hideFlowIndicator = True
        }


type alias ValueColumnOptions =
    { sortable : Bool
    , hideCode : Bool
    , hideFlowIndicator : Bool
    }


valueColumnWithOptions : ValueColumnOptions -> View.Config -> (data -> AssetIdentifier) -> String -> (data -> Api.Data.Values) -> Table.Column data msg
valueColumnWithOptions { sortable, hideCode, hideFlowIndicator } vc getCoinCode name getValues =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> getValues data |> valuesCell vc hideCode hideFlowIndicator (getCoinCode data)
        , sorter =
            if sortable then
                Table.decreasingOrIncreasingBy (\data -> getValues data |> valuesSorter vc (getCoinCode data))

            else
                Table.unsortable
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
    Table.HtmlDetails [ [ PCSS.mGap |> Css.padding, Css.textAlign Css.right ] |> css ]
        [ if hideFlowIndicator then
            none

          else
            flowIndicator
        , text value
        ]

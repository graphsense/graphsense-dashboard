module View.Pathfinder.Table.Columns exposing (CheckboxColumnConfig, ColumnConfig, TwoValuesCellConfig, ValueColumnOptions, checkboxColumn, debitCreditColumn, sortableDebitCreditColumn, stringColumn, timestampDateMultiRowColumn, twoValuesCell, valueColumn, valueColumnWithOptions, wrapCell)

import Api.Data
import Config.View as View
import Css
import Css.Pathfinder as PCSS exposing (inoutStyle)
import Html.Styled exposing (Html, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events
import Model.Currency exposing (AssetIdentifier)
import RecordSetter as Rs
import Table
import Theme.Html.SidePanelComponents as SidePanelComponents
import Tuple exposing (pair)
import Util.Checkbox
import View.Graph.Table exposing (valuesSorter)
import View.Locale as Locale


timestampDateMultiRowColumn : View.Config -> String -> (data -> Int) -> Table.Column data msg
timestampDateMultiRowColumn vc name accessor =
    Table.veryCustomColumn
        { name = name
        , viewData =
            \data ->
                Table.HtmlDetails [ css [ Css.verticalAlign Css.middle ] ]
                    [ let
                        date =
                            accessor data
                                |> Locale.timestampDateUniform vc.locale

                        time =
                            accessor data
                                |> Locale.timestampTimeUniform vc.locale vc.showTimeZoneOffset
                      in
                      SidePanelComponents.sidePanelListTimeCell
                        { root =
                            { date = date
                            , time = time
                            }
                        }
                    ]

        -- , sorter = Table.increasingOrDecreasingBy accessor
        , sorter = Table.unsortable
        }


type alias ColumnConfig data msg =
    { label : String
    , accessor : data -> String
    , onClick : Maybe (data -> msg)
    }


wrapCell : Maybe (data -> msg) -> data -> List (Html msg) -> Table.HtmlDetails msg
wrapCell onClick data =
    Table.HtmlDetails
        (([ Css.verticalAlign Css.middle
          ]
            |> css
         )
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


stringColumn : View.Config -> ColumnConfig data msg -> Table.Column data msg
stringColumn _ { label, accessor } =
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
    , readonly : data -> Bool
    }


checkboxColumn : View.Config -> CheckboxColumnConfig data msg -> Table.Column data msg
checkboxColumn _ { isChecked, onClick, readonly } =
    Table.veryCustomColumn
        { name = ""
        , viewData =
            \data ->
                let
                    attrs =
                        if readonly data then
                            [ [ Css.cursor Css.notAllowed ] |> css ]

                        else
                            []
                in
                Table.HtmlDetails
                    [ [ PCSS.mGap |> Css.padding
                      , Css.width <| Css.px 50
                      ]
                        |> css
                    ]
                    [ Util.Checkbox.checkbox
                        { state = isChecked data |> Util.Checkbox.stateFromBool
                        , size = Util.Checkbox.smallSize
                        , msg = onClick data
                        }
                        attrs
                    ]
        , sorter = Table.unsortable
        }


debitCreditColumn : (data -> Bool) -> View.Config -> (data -> AssetIdentifier) -> String -> (data -> Api.Data.Values) -> Table.Column data msg
debitCreditColumn isOutgoingFn =
    valueColumnWithOptions
        { sortable = False
        , hideCode = False
        , colorFlowDirection = True
        , isOutgoingFn = isOutgoingFn
        }


sortableDebitCreditColumn : (data -> Bool) -> View.Config -> (data -> AssetIdentifier) -> String -> (data -> Api.Data.Values) -> Table.Column data msg
sortableDebitCreditColumn isOutgoingFn =
    valueColumnWithOptions
        { sortable = True
        , hideCode = False
        , colorFlowDirection = True
        , isOutgoingFn = isOutgoingFn
        }


valueColumn : View.Config -> (data -> AssetIdentifier) -> String -> (data -> Api.Data.Values) -> Table.Column data msg
valueColumn =
    valueColumnWithOptions
        { sortable = True
        , hideCode = True
        , colorFlowDirection = False
        , isOutgoingFn = \_ -> False
        }


type alias ValueColumnOptions data =
    { sortable : Bool
    , hideCode : Bool
    , colorFlowDirection : Bool
    , isOutgoingFn : data -> Bool
    }


valueColumnWithOptions : ValueColumnOptions data -> View.Config -> (data -> AssetIdentifier) -> String -> (data -> Api.Data.Values) -> Table.Column data msg
valueColumnWithOptions { sortable, hideCode, colorFlowDirection, isOutgoingFn } vc getCoinCode name getValues =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> getValues data |> valuesCell vc hideCode colorFlowDirection (isOutgoingFn data) (getCoinCode data)
        , sorter =
            if sortable then
                Table.decreasingOrIncreasingBy (\data -> getValues data |> valuesSorter vc (getCoinCode data))

            else
                Table.unsortable
        }


valuesCell : View.Config -> Bool -> Bool -> Bool -> AssetIdentifier -> Api.Data.Values -> Table.HtmlDetails msg
valuesCell vc hideCode colorFlowDirection isOutgoing coinCode values =
    let
        value =
            (if hideCode then
                Locale.currencyWithoutCode

             else
                Locale.currency
            )
                (View.toCurrency vc)
                vc.locale
                [ ( coinCode, values ) ]

        addCss =
            if colorFlowDirection then
                inoutStyle isOutgoing

            else
                []
    in
    Table.HtmlDetails [ css [ Css.verticalAlign Css.middle ] ]
        [ SidePanelComponents.sidePanelListValueCellWithAttributes
            (SidePanelComponents.sidePanelListValueCellAttributes
                |> Rs.s_value [ addCss |> List.map Css.important |> css ]
            )
            { root =
                { value = value
                }
            }
        ]


type alias TwoValuesCellConfig data =
    { coinCode : AssetIdentifier
    , getValue1 : data -> Api.Data.Values
    , getValue2 : data -> Api.Data.Values
    , labelValue2 : String
    }


twoValuesCell : View.Config -> String -> TwoValuesCellConfig data -> Table.Column data msg
twoValuesCell vc name conf =
    let
        toValue =
            pair conf.coinCode
                >> List.singleton
                >> Locale.currencyWithoutCode (View.toCurrency vc) vc.locale
    in
    Table.veryCustomColumn
        { name = name
        , viewData =
            \data ->
                Table.HtmlDetails [ css [ Css.verticalAlign Css.middle ] ]
                    [ SidePanelComponents.sidePanelAddListTwoValuesCell
                        { root =
                            { value1 = conf.getValue1 data |> toValue
                            , value2 = conf.getValue2 data |> toValue
                            , labelValue2 = conf.labelValue2 ++ ":"
                            }
                        }
                    ]
        , sorter = Table.unsortable
        }

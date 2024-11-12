module View.Pathfinder.Table.Columns exposing (CheckboxColumnConfig, ColumnConfig, ValueColumnOptions, checkboxColumn, debitCreditColumn, sortableDebitCreditColumn, stringColumn, timestampDateMultiRowColumn, valueColumn, valueColumnWithOptions, wrapCell)

import Api.Data
import Config.View as View
import Css
import Css.Pathfinder as PCSS exposing (inoutStyle)
import Css.Statusbar
import Html.Styled exposing (Html, span, text)
import Html.Styled.Attributes exposing (css, title)
import Html.Styled.Events
import Model.Currency exposing (AssetIdentifier)
import Model.Pathfinder exposing (HavingTags(..))
import Model.Pathfinder.Id exposing (Id)
import RecordSetter as Rs
import Table
import Theme.Html.Icons as Icons
import Theme.Html.SidePanelComponents as SidePanelComponents
import Util.Pathfinder.TagSummary exposing (hasOnlyExchangeTags, isExchangeNode)
import Util.View exposing (copyIconPathfinder, loadingSpinner, none, truncateLongIdentifierWithLengths)
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
                        { sidePanelListTimeCell =
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
    }


checkboxColumn : View.Config -> CheckboxColumnConfig data msg -> Table.Column data msg
checkboxColumn _ { isChecked, onClick } =
    Table.veryCustomColumn
        { name = ""
        , viewData =
            \data ->
                let
                    attrs =
                        [ onClick data |> Html.Styled.Events.onClick
                        , [ Css.cursor Css.pointer ] |> css
                        ]
                in
                Table.HtmlDetails
                    [ [ PCSS.mGap |> Css.padding
                      , Css.width <| Css.px 50
                      ]
                        |> css
                    ]
                    [ if isChecked data then
                        Icons.checkboxesSize14pxStateSelectedWithAttributes
                            (Icons.checkboxesSize14pxStateSelectedAttributes
                                |> Rs.s_size14pxStateSelected attrs
                            )
                            {}

                      else
                        Icons.checkboxesSize14pxStateDeselectedWithAttributes
                            (Icons.checkboxesSize14pxStateDeselectedAttributes
                                |> Rs.s_size14pxStateDeselected attrs
                            )
                            {}
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
            { sidePanelListValueCell =
                { value = value
                }
            }
        ]

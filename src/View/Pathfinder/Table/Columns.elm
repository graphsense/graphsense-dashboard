module View.Pathfinder.Table.Columns exposing (addressColumn, checkboxColumn, debitCreditColumn, stringColumn, timestampDateMultiRowColumn, txColumn, valueColumn, valueColumnWithOptions)

import Api.Data
import Config.View as View
import Css
import Css.Pathfinder as PCSS exposing (inoutStyle, toAttr)
import Css.Statusbar
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Maybe.Extra
import Model.Currency exposing (AssetIdentifier)
import Model.Pathfinder exposing (HavingTags(..))
import RecordSetter exposing (..)
import Table
import Theme.Html.Icons
import Theme.Html.SidePanelComponents as SidePanelComponents
import Util.View exposing (copyIconPathfinder, copyableLongIdentifierPathfinder, loadingSpinner, none, truncateLongIdentifierWithLengths)
import View.Graph.Table exposing (valuesSorter)
import View.Locale as Locale
import View.Pathfinder.Icons exposing (inIcon, outIcon)


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
    , tagsPlaceholder : Bool
    }


addressColumn : View.Config -> ColumnConfig data msg -> Maybe (data -> HavingTags) -> Table.Column data msg
addressColumn vc cc lblfn =
    identifierColumn (lblfn |> Maybe.withDefault (\_ -> NoTags)) vc cc


identifierColumn : (data -> HavingTags) -> View.Config -> ColumnConfig data msg -> Table.Column data msg
identifierColumn lblfn vc { label, accessor, onClick, tagsPlaceholder } =
    Table.veryCustomColumn
        { name = label
        , viewData =
            \data ->
                SidePanelComponents.sidePanelListIdentifierCellWithTagWithInstances
                    SidePanelComponents.sidePanelListIdentifierCellWithTagAttributes
                    (SidePanelComponents.sidePanelListIdentifierCellWithTagInstances
                        |> s_iconsTagSmall
                            (case lblfn data of
                                LoadingTags ->
                                    span
                                        [ Locale.string vc.locale "Loading tags"
                                            |> title
                                        ]
                                        [ loadingSpinner vc Css.Statusbar.loadingSpinner
                                        ]
                                        |> Just

                                _ ->
                                    Nothing
                            )
                    )
                    { sidePanelListIdentifierCellWithTag =
                        { tagIconVisible =
                            case lblfn data of
                                NoTags ->
                                    False

                                _ ->
                                    True
                        }
                    , sidePanelListIdentifierCell =
                        { copyIconInstance =
                            accessor data |> copyIconPathfinder vc
                        , identifier =
                            accessor data
                                |> truncateLongIdentifierWithLengths 8 4
                        }
                    }
                    |> List.singleton
                    |> Table.HtmlDetails
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

        --, sorter = Table.increasingOrDecreasingBy accessor
        , sorter = Table.unsortable
        }


txColumn : View.Config -> ColumnConfig data msg -> Table.Column data msg
txColumn =
    identifierColumn (\_ -> NoTags)


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


debitCreditColumn : (data -> Bool) -> View.Config -> (data -> AssetIdentifier) -> String -> (data -> Api.Data.Values) -> Table.Column data msg
debitCreditColumn isOutgoingFn =
    valueColumnWithOptions
        { sortable = False
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
                |> s_value [ addCss |> List.map Css.important |> css ]
            )
            { sidePanelListValueCell =
                { value = value
                }
            }
        ]

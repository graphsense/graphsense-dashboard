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
import Table
import Util.View exposing (copyableLongIdentifierPathfinder, loadingSpinner, none)
import View.Graph.Table exposing (valuesSorter)
import View.Locale as Locale
import View.Pathfinder.Icons exposing (inIcon, outIcon)
import View.Pathfinder.Utils exposing (multiLineDateTimeFromTimestamp)


timestampDateMultiRowColumn : View.Config -> String -> (data -> Int) -> Table.Column data msg
timestampDateMultiRowColumn vc name accessor =
    Table.veryCustomColumn
        { name = name
        , viewData =
            \data ->
                Table.HtmlDetails []
                    [ multiLineDateTimeFromTimestamp vc (accessor data)
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
    let
        tagcss =
            [ Css.width (Css.px 15), Css.display Css.inlineBlock ] |> toAttr
    in
    Table.veryCustomColumn
        { name = label
        , viewData =
            \data ->
                (case lblfn data of
                    HasTags ->
                        [ span [ tagcss ] [ FontAwesome.icon FontAwesome.tag |> Html.Styled.fromUnstyled ] ]

                    LoadingTags ->
                        [ span
                            [ Locale.string vc.locale "Loading tags"
                                |> title
                            ]
                            [ loadingSpinner vc Css.Statusbar.loadingSpinner
                            ]
                        ]

                    HasTagSummary ts ->
                        [ span
                            [ tagcss
                            , ts.bestActor
                                |> Maybe.Extra.or ts.bestLabel
                                |> Maybe.withDefault ts.broadCategory
                                |> title
                            ]
                            [ FontAwesome.icon FontAwesome.tag |> Html.Styled.fromUnstyled ]
                        ]

                    NoTags ->
                        [ span
                            (if tagsPlaceholder then
                                [ tagcss ]

                             else
                                []
                            )
                            []
                        ]
                )
                    ++ (accessor data |> copyableLongIdentifierPathfinder vc [] |> List.singleton)
                    |> Table.HtmlDetails
                        (([ PCSS.mGap |> Css.padding ] |> css)
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


debitCreditColumn : View.Config -> (data -> AssetIdentifier) -> String -> (data -> Api.Data.Values) -> Table.Column data msg
debitCreditColumn =
    valueColumnWithOptions
        { sortable = False
        , hideCode = False
        , hideFlowIndicator = True
        , colorFlowDirection = True
        }


valueColumn : View.Config -> (data -> AssetIdentifier) -> String -> (data -> Api.Data.Values) -> Table.Column data msg
valueColumn =
    valueColumnWithOptions
        { sortable = True
        , hideCode = True
        , hideFlowIndicator = True
        , colorFlowDirection = False
        }


type alias ValueColumnOptions =
    { sortable : Bool
    , hideCode : Bool
    , hideFlowIndicator : Bool
    , colorFlowDirection : Bool
    }


valueColumnWithOptions : ValueColumnOptions -> View.Config -> (data -> AssetIdentifier) -> String -> (data -> Api.Data.Values) -> Table.Column data msg
valueColumnWithOptions { sortable, hideCode, hideFlowIndicator, colorFlowDirection } vc getCoinCode name getValues =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> getValues data |> valuesCell vc hideCode hideFlowIndicator colorFlowDirection (getCoinCode data)
        , sorter =
            if sortable then
                Table.decreasingOrIncreasingBy (\data -> getValues data |> valuesSorter vc (getCoinCode data))

            else
                Table.unsortable
        }


valuesCell : View.Config -> Bool -> Bool -> Bool -> AssetIdentifier -> Api.Data.Values -> Table.HtmlDetails msg
valuesCell vc hideCode hideFlowIndicator colorFlowDirection coinCode values =
    let
        value =
            (if hideCode then
                Locale.currencyWithoutCode

             else
                Locale.currency
            )
                vc.locale
                [ ( coinCode, values ) ]

        isOutFlow =
            String.startsWith "-" value

        flowIndicator =
            if isOutFlow then
                outIcon

            else
                inIcon

        addCss =
            if colorFlowDirection then
                inoutStyle isOutFlow

            else
                []
    in
    Table.HtmlDetails [ ([ PCSS.mGap |> Css.padding, Css.textAlign Css.right ] ++ addCss) |> css ]
        [ if hideFlowIndicator then
            none

          else
            flowIndicator
        , text value
        ]

module View.Pathfinder.PagedTable exposing (customizations, pagedTableView)

import Components.PagedTable as PagedTable
import Config.View as View
import Css
import Css.Pathfinder exposing (emptyTableMsg, fullWidth)
import Css.Table exposing (Styles, loadingSpinner, styles)
import Html.Styled exposing (Attribute, Html, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import RecordSetter as Rs
import Table
import Theme.Colors
import Theme.Html.Icons as Icons
import Theme.Html.SidePanelComponents as SidePanelComponents
import Tuple3
import Util.Css
import Util.View
import View.Graph.Table exposing (simpleThead)
import View.Locale as Locale


tableHint : Styles -> View.Config -> String -> Html msg
tableHint _ vc msg =
    div
        [ emptyTableMsg |> css
        ]
        [ Locale.string vc.locale msg |> text
        ]


customizations : View.Config -> Table.Customizations data msg
customizations vc =
    Table.defaultCustomizations
        |> Rs.s_tableAttrs [ fullWidth ++ Css.Table.table vc |> css ]
        |> Rs.s_thead (List.map (Tuple3.mapThird List.singleton) >> simpleThead styles vc)
        |> Rs.s_rowAttrs (\_ -> [ Css.Table.row vc |> css ])



-- rawTableView : View.Config -> List (Attribute msg) -> Table.Config data msg -> String -> List data -> Html msg
-- rawTableView _ attributes config sortColumn data =
--     div []
--         [ div attributes
--             [ Table.view config (Table.initialSort sortColumn) data ]
--         ]


pagedTableView : View.Config -> List (Attribute msg) -> Table.Config data msg -> PagedTable.Model data -> (PagedTable.Msg -> msg) -> Html msg
pagedTableView vc attributes config tblPaged msgTag =
    let
        tbl =
            PagedTable.getTable tblPaged

        filteredData =
            PagedTable.getPage tblPaged

        nextPageAvailable =
            PagedTable.hasNextPage tblPaged

        overrideColor =
            Util.Css.overrideBlack Theme.Colors.greyBlue500

        nextActiveAttributes =
            if nextPageAvailable then
                [ onClick (PagedTable.NextPage |> msgTag)
                , [ Css.cursor Css.pointer
                  , overrideColor
                  ]
                    |> css
                ]

            else
                []

        prevActiveAttributes =
            if PagedTable.hasPrevPage tblPaged then
                [ onClick (PagedTable.PrevPage |> msgTag)
                , [ Css.cursor Css.pointer
                  , overrideColor
                  ]
                    |> css
                ]

            else
                []

        firstActiveAttributes =
            if PagedTable.hasPrevPage tblPaged then
                [ onClick (PagedTable.FirstPage |> msgTag), [ Css.cursor Css.pointer ] |> css ]

            else
                []

        paggingBlockAttributes =
            [ [ Css.width (Css.pct 100)
              ]
                |> css
            , Util.View.noTextSelection
            ]

        { listPartState, leftDisabled, rightDisabled } =
            if PagedTable.getCurrentPage tblPaged == 1 && nextPageAvailable then
                { listPartState = SidePanelComponents.PaginationListPartStart
                , leftDisabled = True
                , rightDisabled = False
                }

            else if PagedTable.getCurrentPage tblPaged == 1 && not nextPageAvailable then
                { listPartState = SidePanelComponents.PaginationListPartOnePage
                , leftDisabled = True
                , rightDisabled = True
                }

            else if nextPageAvailable then
                { listPartState = SidePanelComponents.PaginationListPartMiddle
                , leftDisabled = False
                , rightDisabled = False
                }

            else
                { listPartState = SidePanelComponents.PaginationListPartEnd
                , leftDisabled = False
                , rightDisabled = True
                }

        listPart =
            { nextLabel = Locale.string vc.locale "Next"
            , previousLabel = Locale.string vc.locale "Previous"
            , pageNumberLabel =
                PagedTable.getCurrentPage tblPaged
                    |> String.fromInt
                    |> (++) (Locale.string vc.locale "Page" ++ " ")
            , listPart = listPartState
            }

        chevronLeft =
            Icons.iconsChevronLeftThin
                { root =
                    { state =
                        if leftDisabled then
                            Icons.IconsChevronLeftThinStateDisabled

                        else
                            Icons.IconsChevronLeftThinStateDefault
                    }
                }

        chevronRight =
            Icons.iconsChevronRightThin
                { root =
                    { state =
                        if rightDisabled then
                            Icons.IconsChevronRightThinStateDisabled

                        else
                            Icons.IconsChevronRightThinStateDefault
                    }
                }

        wrapNote =
            List.singleton
                >> div
                    [ css
                        [ Css.flexGrow <| Css.num 1
                        ]
                    ]
                >> List.singleton
    in
    div
        (css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.justifyContent Css.spaceBetween
            ]
            :: attributes
        )
        (Table.view config tbl.state filteredData
            :: (if tbl.loading then
                    Util.View.loadingSpinner vc loadingSpinner
                        |> wrapNote

                else if List.isEmpty tbl.data then
                    tableHint styles vc "No records found"
                        |> wrapNote

                else if List.isEmpty filteredData then
                    tableHint styles vc "No rows match your filter criteria"
                        |> wrapNote

                else
                    []
               )
            ++ [ SidePanelComponents.paginationWithAttributes
                    (SidePanelComponents.paginationAttributes
                        |> Rs.s_root paggingBlockAttributes
                        |> Rs.s_nextButton nextActiveAttributes
                        |> Rs.s_iconsChevronLeftEnd firstActiveAttributes
                        |> Rs.s_previousButton prevActiveAttributes
                    )
                    { root = listPart
                    , iconsChevronLeftThin = { variant = chevronLeft }
                    , iconsChevronRightThin = { variant = chevronRight }
                    }
               ]
        )

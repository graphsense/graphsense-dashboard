module View.Pathfinder.PagedTable exposing (ColumnAlign(..), alignColumnHeader, customizations, pagedTableView)

import Config.View as View
import Css
import Css.Pathfinder exposing (emptyTableMsg, fullWidth)
import Css.Table exposing (Styles, loadingSpinner, styles)
import Dict exposing (Dict)
import Html.Styled exposing (Attribute, Html, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import PagedTable
import RecordSetter as Rs
import Table
import Theme.Html.Icons as Icons
import Theme.Html.SidePanelComponents as SidePanelComponents
import Tuple3
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


type ColumnAlign
    = LeftAligned
    | CenterAligned
    | RightAligned


alignColumnHeader : Styles -> View.Config -> Dict String ColumnAlign -> Table.Customizations data msg -> Table.Customizations data msg
alignColumnHeader styles_ vc columns tc =
    let
        addAttr ( name, x, attr ) =
            ( name
            , x
            , case Dict.get name columns of
                Just LeftAligned ->
                    ([ Css.textAlign Css.left ] |> css) :: attr

                Just CenterAligned ->
                    ([ Css.textAlign Css.center ] |> css) :: attr

                Just RightAligned ->
                    ([ Css.textAlign Css.right ] |> css) :: attr

                _ ->
                    attr
            )
    in
    tc |> Rs.s_thead (List.map (Tuple3.mapThird List.singleton) >> List.map addAttr >> simpleThead styles_ vc)


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

        nextActiveAttributes =
            if nextPageAvailable then
                [ onClick (PagedTable.NextPage |> msgTag), [ Css.cursor Css.pointer ] |> css ]

            else
                []

        prevActiveAttributes =
            if PagedTable.hasPrevPage tblPaged then
                [ onClick (PagedTable.PrevPage |> msgTag), [ Css.cursor Css.pointer ] |> css ]

            else
                []

        firstActiveAttributes =
            if PagedTable.hasPrevPage tblPaged then
                [ onClick (PagedTable.FirstPage |> msgTag), [ Css.cursor Css.pointer ] |> css ]

            else
                []

        paggingBlockAttributes =
            [ [ Css.width (Css.pct 100) ] |> css, Util.View.noTextSelection ]

        listPart =
            { nextLabel = Locale.string vc.locale "Next"
            , previousLabel = Locale.string vc.locale "Previous"
            , pageNumberLabel =
                PagedTable.getCurrentPage tblPaged
                    |> String.fromInt
                    |> (++) (Locale.string vc.locale "Page" ++ " ")
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
            ++ [ if PagedTable.getCurrentPage tblPaged == 1 && nextPageAvailable then
                    SidePanelComponents.paginationListPartStartWithInstances
                        (SidePanelComponents.paginationListPartStartAttributes
                            |> Rs.s_listPartStart paggingBlockAttributes
                            |> Rs.s_next nextActiveAttributes
                        )
                        (SidePanelComponents.paginationListPartStartInstances
                         -- |> s_iconsChevronRightEnd (Just Util.View.none)
                        )
                        { listPartStart = listPart
                        , iconsChevronLeftThin =
                            { variant = Icons.iconsChevronLeftThinStateDisabled {} }
                        , iconsChevronRightThin =
                            { variant =
                                Icons.iconsChevronRightThinStateDefaultWithAttributes
                                    (Icons.iconsChevronRightThinStateDefaultAttributes
                                        |> Rs.s_stateDefault nextActiveAttributes
                                    )
                                    {}
                            }
                        }

                 else if PagedTable.getCurrentPage tblPaged == 1 && not nextPageAvailable then
                    SidePanelComponents.paginationListPartOnePageWithInstances
                        (SidePanelComponents.paginationListPartOnePageAttributes
                            |> Rs.s_listPartOnePage paggingBlockAttributes
                            |> Rs.s_next nextActiveAttributes
                        )
                        (SidePanelComponents.paginationListPartOnePageInstances
                         -- |> s_iconsChevronRightEnd (Just Util.View.none)
                        )
                        { listPartOnePage = listPart
                        , iconsChevronLeftThin =
                            { variant = Icons.iconsChevronLeftThinStateDisabled {} }
                        , iconsChevronRightThin =
                            { variant = Icons.iconsChevronRightThinStateDisabled {} }
                        }

                 else if nextPageAvailable then
                    SidePanelComponents.paginationListPartMiddleWithInstances
                        (SidePanelComponents.paginationListPartMiddleAttributes
                            |> Rs.s_listPartMiddle paggingBlockAttributes
                            |> Rs.s_iconsChevronLeftEnd firstActiveAttributes
                            |> Rs.s_next nextActiveAttributes
                            |> Rs.s_previous prevActiveAttributes
                        )
                        (SidePanelComponents.paginationListPartMiddleInstances
                         -- |> s_iconsChevronRightEnd (Just Util.View.none)
                        )
                        { listPartMiddle = listPart
                        , iconsChevronLeftThin =
                            { variant =
                                Icons.iconsChevronLeftThinStateDefaultWithAttributes
                                    (Icons.iconsChevronLeftThinStateDefaultAttributes
                                        |> Rs.s_stateDefault prevActiveAttributes
                                    )
                                    {}
                            }
                        , iconsChevronRightThin =
                            { variant =
                                Icons.iconsChevronRightThinStateDefaultWithAttributes
                                    (Icons.iconsChevronRightThinStateDefaultAttributes
                                        |> Rs.s_stateDefault nextActiveAttributes
                                    )
                                    {}
                            }
                        }

                 else
                    SidePanelComponents.paginationListPartEndWithInstances
                        (SidePanelComponents.paginationListPartEndAttributes
                            |> Rs.s_listPartEnd paggingBlockAttributes
                            |> Rs.s_iconsChevronLeftEnd firstActiveAttributes
                            |> Rs.s_previous prevActiveAttributes
                        )
                        (SidePanelComponents.paginationListPartEndInstances
                         -- |> s_iconsChevronRightEnd (Just Util.View.none)
                        )
                        { listPartEnd = listPart
                        , iconsChevronLeftThin =
                            { variant =
                                Icons.iconsChevronLeftThinStateDefaultWithAttributes
                                    (Icons.iconsChevronLeftThinStateDefaultAttributes
                                        |> Rs.s_stateDefault prevActiveAttributes
                                    )
                                    {}
                            }
                        , iconsChevronRightThin =
                            { variant = Icons.iconsChevronRightThinStateDisabled {} }
                        }
               ]
        )

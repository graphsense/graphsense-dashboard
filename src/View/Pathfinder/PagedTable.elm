module View.Pathfinder.PagedTable exposing (alignColumnsRight, customizations, pagedTableView)

import Config.View as View
import Css
import Css.Pathfinder exposing (emptyTableMsg, fullWidth)
import Css.Table exposing (Styles, loadingSpinner, styles)
import Html.Styled exposing (Html, div, text, Attribute)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Model.Pathfinder.PagedTable as PT exposing (PagedTable)
import RecordSetter as Rs
import Set exposing (Set)
import Table
import Theme.Html.SidePanelComponents as SidePanelComponents
import Tuple3
import Util.View
import View.Graph.Table exposing (simpleThead)
import View.Locale as Locale


type alias PagingMsg data msg =
    PagedTable data -> msg


tableHint : Styles -> View.Config -> String -> Html msg
tableHint styles vc msg =
    div
        [ emptyTableMsg |> css
        ]
        [ Locale.string vc.locale msg |> text
        ]


alignColumnsRight : View.Config -> Set String -> Table.Customizations data msg -> Table.Customizations data msg
alignColumnsRight vc columns tc =
    let
        addAttr ( name, x, attr ) =
            ( name
            , x
            , if Set.member name columns then
                ([ Css.textAlign Css.right ] |> css) :: attr

              else
                attr
            )
    in
    tc |> Rs.s_thead (List.map (Tuple3.mapThird List.singleton) >> List.map addAttr >> simpleThead styles vc)


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


pagedTableView : View.Config -> List (Attribute msg) -> Table.Config data msg -> PagedTable data -> PagingMsg data msg -> PagingMsg data msg -> PagingMsg data msg -> Html msg
pagedTableView vc attributes config tblPaged prevMsg nextMsg firstMsg =
    let
        tbl =
            tblPaged.table

        filteredData =
            PT.getPage tblPaged

        nextPageAvailable =
            PT.hasNextPage tblPaged

        nextActiveAttributes =
            if nextPageAvailable then
                [ onClick (nextMsg tblPaged), [ Css.cursor Css.pointer ] |> css ]

            else
                []

        prevActiveAttributes =
            if tblPaged.currentPage > 1 then
                [ onClick (prevMsg tblPaged), [ Css.cursor Css.pointer ] |> css ]

            else
                []

        firstActiveAttributes =
            if tblPaged.currentPage > 1 then
                [ onClick (firstMsg tblPaged), [ Css.cursor Css.pointer ] |> css ]

            else
                []

        paggingBlockAttributes =
            [ [ Css.width (Css.pct 100) ] |> css ]

        listPart =
            { nextLabel = Locale.string vc.locale "Next"
            , previousLabel = Locale.string vc.locale "Previous"
            , pageNumberLabel =
                tblPaged.currentPage
                    |> String.fromInt
                    |> (++) (Locale.string vc.locale "Page" ++ " ")
            }
    in
    div
        []
        [ div
            attributes
            [ Table.view config tbl.state filteredData
            , if tbl.loading then
                Util.View.loadingSpinner vc loadingSpinner

              else if List.isEmpty tbl.data then
                tableHint styles vc "No records found"

              else if List.isEmpty filteredData then
                tableHint styles vc "No rows match your filter criteria"

              else
                Util.View.none
            , if tblPaged.currentPage == 1 && nextPageAvailable then
                SidePanelComponents.paginationListPartStartWithInstances
                    (SidePanelComponents.paginationListPartStartAttributes
                        |> Rs.s_listPartStart paggingBlockAttributes
                        |> Rs.s_iconsChevronRightThin nextActiveAttributes
                        |> Rs.s_next nextActiveAttributes
                    )
                    (SidePanelComponents.paginationListPartStartInstances
                     -- |> s_iconsChevronRightEnd (Just Util.View.none)
                    )
                    { listPartStart = listPart }

              else if tblPaged.currentPage == 1 && not nextPageAvailable then
                SidePanelComponents.paginationListPartOnePageWithInstances
                    (SidePanelComponents.paginationListPartOnePageAttributes
                        |> Rs.s_listPartOnePage paggingBlockAttributes
                        |> Rs.s_iconsChevronRightThin nextActiveAttributes
                        |> Rs.s_next nextActiveAttributes
                    )
                    (SidePanelComponents.paginationListPartOnePageInstances
                     -- |> s_iconsChevronRightEnd (Just Util.View.none)
                    )
                    { listPartOnePage = listPart }

              else if nextPageAvailable then
                SidePanelComponents.paginationListPartMiddleWithInstances
                    (SidePanelComponents.paginationListPartMiddleAttributes
                        |> Rs.s_listPartMiddle paggingBlockAttributes
                        |> Rs.s_iconsChevronRightThin nextActiveAttributes
                        |> Rs.s_iconsChevronLeftThin prevActiveAttributes
                        |> Rs.s_iconsChevronLeftEnd firstActiveAttributes
                        |> Rs.s_next nextActiveAttributes
                        |> Rs.s_previous prevActiveAttributes
                    )
                    (SidePanelComponents.paginationListPartMiddleInstances
                     -- |> s_iconsChevronRightEnd (Just Util.View.none)
                    )
                    { listPartMiddle = listPart }

              else
                SidePanelComponents.paginationListPartEndWithInstances
                    (SidePanelComponents.paginationListPartEndAttributes
                        |> Rs.s_listPartEnd paggingBlockAttributes
                        |> Rs.s_nextCell nextActiveAttributes
                        |> Rs.s_next nextActiveAttributes
                        |> Rs.s_iconsChevronLeftThin prevActiveAttributes
                        |> Rs.s_iconsChevronLeftEnd firstActiveAttributes
                        |> Rs.s_previous prevActiveAttributes
                    )
                    (SidePanelComponents.paginationListPartEndInstances
                     -- |> s_iconsChevronRightEnd (Just Util.View.none)
                    )
                    { listPartEnd = listPart }
            ]
        ]

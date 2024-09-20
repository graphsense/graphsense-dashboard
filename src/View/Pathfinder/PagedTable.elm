module View.Pathfinder.PagedTable exposing (alignColumnsRight, customizations, pagedTableView, rawTableView)

import Config.View as View
import Css
import Css.Pathfinder exposing (centerContent, emptyTableMsg, fullWidth, linkButtonStyle, toAttr)
import Css.Table exposing (Styles, loadingSpinner, styles)
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Model exposing (Msg(..))
import Model.Pathfinder.PagedTable as PT exposing (PagedTable)
import RecordSetter exposing (..)
import Set
import Table
import Theme.Html.Icons as HIcons
import Theme.Html.SidePanelComponents as SidePanelComponents
import Tuple3
import Util.View
import View.Graph.Table exposing (simpleThead, tableHint)
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


alignColumnsRight : View.Config -> Set.Set String -> Table.Customizations data msg -> Table.Customizations data msg
alignColumnsRight vc columns tc =
    let
        addAttr ( name, x, attr ) =
            ( name
            , x
            , if Set.member name columns then
                ([ Css.textAlign Css.right ] |> HA.css) :: attr

              else
                attr
            )
    in
    tc |> s_thead (List.map (Tuple3.mapThird List.singleton) >> List.map addAttr >> simpleThead styles vc)


customizations : View.Config -> Table.Customizations data msg
customizations vc =
    Table.defaultCustomizations
        |> s_tableAttrs [ fullWidth ++ Css.Table.table vc |> css ]
        |> s_thead (List.map (Tuple3.mapThird List.singleton) >> simpleThead styles vc)
        |> s_rowAttrs (\_ -> [ Css.Table.row vc |> css ])


rawTableView : View.Config -> List (Attribute msg) -> Table.Config data msg -> String -> List data -> Html msg
rawTableView _ attributes config sortColumn data =
    div []
        [ div attributes
            [ Table.view config (Table.initialSort sortColumn) data ]
        ]


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
                        |> s_listPartStart paggingBlockAttributes
                        |> s_iconsChevronRightThin nextActiveAttributes
                    )
                    (SidePanelComponents.paginationListPartStartInstances
                     -- |> s_iconsChevronRightEnd (Just Util.View.none)
                    )
                    { listPartStart = listPart }

              else if tblPaged.currentPage == 1 && not nextPageAvailable then
                SidePanelComponents.paginationListPartOnePageWithInstances
                    (SidePanelComponents.paginationListPartOnePageAttributes
                        |> s_listPartOnePage paggingBlockAttributes
                        |> s_iconsChevronRightThin nextActiveAttributes
                    )
                    (SidePanelComponents.paginationListPartOnePageInstances
                     -- |> s_iconsChevronRightEnd (Just Util.View.none)
                    )
                    { listPartOnePage = listPart }

              else if nextPageAvailable then
                SidePanelComponents.paginationListPartMiddleWithInstances
                    (SidePanelComponents.paginationListPartMiddleAttributes
                        |> s_listPartMiddle paggingBlockAttributes
                        |> s_iconsChevronRightThin nextActiveAttributes
                        |> s_iconsChevronLeftThin prevActiveAttributes
                        |> s_iconsChevronLeftEnd firstActiveAttributes
                    )
                    (SidePanelComponents.paginationListPartMiddleInstances
                     -- |> s_iconsChevronRightEnd (Just Util.View.none)
                    )
                    { listPartMiddle = listPart }

              else
                SidePanelComponents.paginationListPartEndWithInstances
                    (SidePanelComponents.paginationListPartEndAttributes
                        |> s_listPartEnd paggingBlockAttributes
                        |> s_nextCell nextActiveAttributes
                        |> s_iconsChevronLeftThin prevActiveAttributes
                        |> s_iconsChevronLeftEnd firstActiveAttributes
                    )
                    (SidePanelComponents.paginationListPartEndInstances
                     -- |> s_iconsChevronRightEnd (Just Util.View.none)
                    )
                    { listPartEnd = listPart }
            ]
        ]

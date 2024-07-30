module View.Pathfinder.PagedTable exposing (alignColumnsRight, customizations, pagedTableView, rawTableView)

import Config.View as View
import Css
import Css.Pathfinder exposing (centerContent, fullWidth, linkButtonStyle, toAttr)
import Css.Table exposing (loadingSpinner, styles)
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Model.Pathfinder.PagedTable as PT exposing (PagedTable)
import RecordSetter exposing (s_rowAttrs, s_tableAttrs, s_thead)
import Set
import Table
import Tuple3
import Util.View
import View.Graph.Table exposing (simpleThead, tableHint)


type alias PagingMsg data msg =
    PagedTable data -> msg


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


pageIndicatorView : PagedTable data -> Html msg
pageIndicatorView pt =
    let
        pageText =
            pt.currentPage |> String.fromInt

        -- ++ " of " ++ (PT.nrPages pt |> Maybe.map String.fromInt |> Maybe.withDefault "?")
    in
    span [] [ text pageText ]


rawTableView : View.Config -> List (Attribute msg) -> Table.Config data msg -> String -> List data -> Html msg
rawTableView _ attributes config sortColumn data =
    div []
        [ div attributes
            [ Table.view config (Table.initialSort sortColumn) data ]
        ]


pagedTableView : View.Config -> List (Attribute msg) -> Table.Config data msg -> PagedTable data -> PagingMsg data msg -> PagingMsg data msg -> Html msg
pagedTableView vc attributes config tblPaged prevMsg nextMsg =
    let
        tbl =
            tblPaged.table

        filteredData =
            PT.getPage tblPaged

        nextPageAvailable =
            PT.hasNextPage tblPaged
    in
    div
        []
        [ div
            attributes
            (Table.view config tbl.state filteredData
                :: (if tbl.loading then
                        [ Util.View.loadingSpinner vc loadingSpinner
                        ]

                    else if List.isEmpty tbl.data then
                        [ tableHint styles vc "No records found"
                        ]

                    else if List.isEmpty filteredData then
                        [ tableHint styles vc "No rows match your filter criteria"
                        ]

                    else
                        [ div [ centerContent |> toAttr ]
                            [ div []
                                [ button [ linkButtonStyle vc (tblPaged.currentPage > 1) |> toAttr, onClick (prevMsg tblPaged) ] [ text "<" ]
                                , pageIndicatorView tblPaged
                                , button [ linkButtonStyle vc nextPageAvailable |> toAttr, onClick (nextMsg tblPaged) ] [ text ">" ]
                                ]
                            ]
                        ]
                   )
            )
        ]

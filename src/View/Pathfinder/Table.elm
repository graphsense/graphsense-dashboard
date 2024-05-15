module View.Pathfinder.Table exposing (pagedTableView)

import Config.View as View
import Css.Pathfinder exposing (centerContent, linkButtonStyle, toAttr)
import Css.Table exposing (loadingSpinner)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Pathfinder.Table as PT exposing (PagedTable)
import Table
import Util.View
import View.Graph.Table exposing (Tools, tableHint)


type alias PagingMsg data msg =
    PagedTable data -> msg


pageIndicatorView : PagedTable data -> Html msg
pageIndicatorView pt =
    let
        pageText =
            (pt.currentPage |> String.fromInt) ++ " of " ++ (PT.nrPages pt |> Maybe.map String.fromInt |> Maybe.withDefault "?")
    in
    span [] [ text pageText ]


pagedTableView : View.Config -> List (Attribute msg) -> Tools msg -> Table.Config data msg -> PagedTable data -> PagingMsg data msg -> PagingMsg data msg -> Html msg
pagedTableView vc attributes _ config tblPaged prevMsg nextMsg =
    let
        tbl =
            tblPaged.t

        max_page =
            PT.nrPages tblPaged

        filteredData =
            PT.getPage tblPaged
    in
    div
        [ Css.Table.root vc |> css
        ]
        [ div
            attributes
            (Table.view config tbl.state filteredData
                :: (if tbl.loading then
                        [ Util.View.loadingSpinner vc loadingSpinner
                        ]

                    else if List.isEmpty tbl.data then
                        [ tableHint vc "No records found"
                        ]

                    else if List.isEmpty filteredData then
                        [ tableHint vc "No rows match your filter criteria"
                        ]

                    else
                        [ div [ centerContent |> toAttr ]
                            [ div []
                                [ button [ linkButtonStyle vc (tblPaged.currentPage > 1) |> toAttr, onClick (prevMsg tblPaged) ] [ text "<" ]
                                , pageIndicatorView tblPaged
                                , button [ linkButtonStyle vc (max_page |> Maybe.map (\x -> tblPaged.currentPage < x) |> Maybe.withDefault True) |> toAttr, onClick (nextMsg tblPaged) ] [ text ">" ]
                                ]
                            ]
                        ]
                   )
            )
        ]

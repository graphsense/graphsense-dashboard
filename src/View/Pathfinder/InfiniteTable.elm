module View.Pathfinder.InfiniteTable exposing (Config, view)

import Components.InfiniteTable as InfiniteTable
import Config.View as View
import Css
import Css.Pathfinder exposing (emptyTableMsg)
import Css.Table exposing (loadingSpinner)
import Html.Styled exposing (Attribute, Html, div, text)
import Html.Styled.Attributes exposing (css)
import InfiniteScroll
import Table
import Util.View
import View.Locale as Locale


type alias Config msg =
    { infiniteScrollMsg : InfiniteScroll.Msg -> msg
    }


tableHint : View.Config -> String -> Html msg
tableHint vc msg =
    div
        [ emptyTableMsg |> css
        ]
        [ Locale.string vc.locale msg |> text
        ]


view : View.Config -> List (Attribute msg) -> Table.Config data msg -> (InfiniteTable.Msg -> msg) -> InfiniteTable.Model data -> Html msg
view vc attributes config infiniteScrollMsg tblInfinite =
    let
        tbl =
            InfiniteTable.getTable tblInfinite

        filteredData =
            tbl.filtered

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
            , Css.maxHeight <| Css.px 300
            , Css.overflowY Css.auto
            ]
            :: InfiniteTable.infiniteScroll infiniteScrollMsg
            :: attributes
        )
        (Table.view config tbl.state filteredData
            :: (if tbl.loading then
                    Util.View.loadingSpinner vc loadingSpinner
                        |> wrapNote

                else if List.isEmpty tbl.data then
                    tableHint vc "No records found"
                        |> wrapNote

                else if List.isEmpty filteredData then
                    tableHint vc "No rows match your filter criteria"
                        |> wrapNote

                else
                    []
               )
        )

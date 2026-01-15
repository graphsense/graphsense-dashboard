module View.Pathfinder.InfiniteTable exposing (Config, loadingPlaceholderAbove, loadingPlaceholderBelow, view)

import Components.InfiniteTable as InfiniteTable
import Config.View as View
import Css
import Css.Pathfinder exposing (emptyTableMsg)
import Css.Table exposing (loadingSpinner)
import Html.Styled exposing (Attribute, Html, div, text)
import Html.Styled.Attributes exposing (css)
import InfiniteScroll
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


view : View.Config -> List (Attribute msg) -> InfiniteTable.TableConfig data msg -> InfiniteTable.Model data -> Html msg
view vc attributes config tblInfinite =
    div
        (css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.justifyContent Css.spaceBetween
            ]
            :: attributes
        )
        [ if InfiniteTable.isEmpty tblInfinite then
            div
                [ css
                    [ Css.flexGrow <| Css.num 1
                    ]
                ]
                (if InfiniteTable.isLoading tblInfinite then
                    loadingPlaceholderBelow vc

                 else
                    [ tableHint vc "no records found" ]
                )

          else
            InfiniteTable.view config [ css [ Css.maxHeight <| Css.px 300 ] ] tblInfinite
        ]


loadingPlaceholderAbove : View.Config -> List (Html msg)
loadingPlaceholderAbove vc =
    Util.View.loadingSpinner vc loadingSpinner
        |> List.singleton
        |> div
            [ css
                [ Css.alignItems Css.flexEnd
                , Css.displayFlex
                , Css.height <| Css.pct 100
                ]
            ]
        |> List.singleton


loadingPlaceholderBelow : View.Config -> List (Html msg)
loadingPlaceholderBelow vc =
    Util.View.loadingSpinner vc loadingSpinner
        |> List.singleton

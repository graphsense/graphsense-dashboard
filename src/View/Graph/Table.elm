module View.Graph.Table exposing (Tools, customizations, htmlColumn, htmlColumnWithSorter, intColumn, noTools, simpleThead, simpleTheadHelp, stringColumn, table, valuesSorter)

import Api.Data
import Components.Table as T
import Config.View as View
import Css.Table exposing (Styles)
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model.Currency exposing (AssetIdentifier)
import RecordSetter exposing (..)
import Table
import Tuple exposing (..)
import Tuple3
import Util.View.Loadingspinner as Loadingspinner
import View.Locale as Locale


type alias Tools msg =
    { filter : Maybe (Maybe String -> msg)
    , csv : Maybe msg
    }


noTools : Tools msg
noTools =
    { filter = Nothing
    , csv = Nothing
    }


table : Styles -> View.Config -> List (Attribute msg) -> Table.Config data msg -> T.Table data -> Html msg
table styles vc attributes config tbl =
    div
        [ styles.root vc |> css
        ]
        [ div
            ((styles.tableRoot vc |> css) :: attributes)
            (Table.view config tbl.state tbl.filtered
                :: (if tbl.loading then
                        [ Loadingspinner.html [ css (styles.loadingSpinner vc) ]
                        ]

                    else if List.isEmpty tbl.data then
                        [ tableHint styles vc "This table is empty"
                        ]

                    else if List.isEmpty tbl.filtered then
                        [ tableHint styles vc "Table-no-rows-match-filter"
                        ]

                    else
                        []
                   )
            )
        ]


customizations : Styles -> View.Config -> Table.Customizations data msg
customizations styles vc =
    Table.defaultCustomizations
        |> s_tableAttrs [ styles.table vc |> css ]
        |> s_thead (List.map (Tuple3.mapThird List.singleton) >> simpleThead styles vc)
        |> s_rowAttrs (\_ -> [ styles.row vc |> css ])


simpleThead : Styles -> View.Config -> List ( String, Table.Status, List (Attribute msg) ) -> Table.HtmlDetails msg
simpleThead styles vc headers =
    Table.HtmlDetails [ styles.headRow vc |> css ] (List.map (simpleTheadHelp styles vc) headers)


simpleTheadHelp : Styles -> View.Config -> ( String, Table.Status, List (Attribute msg) ) -> Html msg
simpleTheadHelp styles vc ( name, status, attrs ) =
    let
        n =
            Locale.string vc.locale name
                |> text

        withCss =
            pair [ styles.headCellSortable vc |> css ]

        ( attr, content ) =
            case status of
                Table.Unsortable ->
                    ( [], [ n ] )

                Table.Sortable selected ->
                    [ n
                    , text " "
                    , if selected then
                        FontAwesome.sortUp
                            |> FontAwesome.icon
                            |> Html.Styled.fromUnstyled

                      else
                        FontAwesome.sortDown
                            |> FontAwesome.icon
                            |> Html.Styled.fromUnstyled
                    ]
                        |> withCss

                Table.Reversible Nothing ->
                    [ n
                    , text " "
                    , FontAwesome.sort
                        |> FontAwesome.icon
                        |> Html.Styled.fromUnstyled
                    ]
                        |> withCss

                Table.Reversible (Just isReversed) ->
                    [ n
                    , text " "
                    , if isReversed then
                        FontAwesome.sortUp
                            |> FontAwesome.icon
                            |> Html.Styled.fromUnstyled

                      else
                        FontAwesome.sortDown
                            |> FontAwesome.icon
                            |> Html.Styled.fromUnstyled
                    ]
                        |> withCss
    in
    div attr content
        |> List.singleton
        |> th
            ((styles.headCell vc |> css) :: attrs)


htmlColumn : Styles -> View.Config -> String -> (data -> comparable) -> (data -> List (Html msg)) -> Table.Column data msg
htmlColumn styles vc name accessor html =
    htmlColumnWithSorter (Table.increasingOrDecreasingBy accessor) styles vc name accessor html


htmlColumnWithSorter : Table.Sorter data -> Styles -> View.Config -> String -> (data -> comparable) -> (data -> List (Html msg)) -> Table.Column data msg
htmlColumnWithSorter sorter styles vc name _ html =
    Table.veryCustomColumn
        { name = name
        , viewData = html >> Table.HtmlDetails [ styles.cell vc |> css ]
        , sorter = sorter
        }


stringColumn : Styles -> View.Config -> String -> (data -> String) -> Table.Column data msg
stringColumn styles vc name accessor =
    Table.veryCustomColumn
        { name = name
        , viewData = accessor >> text >> List.singleton >> Table.HtmlDetails [ styles.cell vc |> css ]
        , sorter = Table.increasingOrDecreasingBy accessor
        }


intColumn : Styles -> View.Config -> String -> (data -> Int) -> Table.Column data msg
intColumn styles vc name accessor =
    Table.veryCustomColumn
        { name = name
        , viewData =
            accessor
                >> Locale.int vc.locale
                >> text
                >> List.singleton
                >> Table.HtmlDetails [ styles.numberCell vc |> css ]
        , sorter = Table.increasingOrDecreasingBy accessor
        }


valuesSorter : View.Config -> AssetIdentifier -> Api.Data.Values -> Float
valuesSorter vc asset values =
    Locale.valuesToFloat (View.toCurrency vc) vc.locale asset values
        |> Maybe.withDefault 0


tableHint : Styles -> View.Config -> String -> Html msg
tableHint styles vc msg =
    div
        [ styles.emptyHint vc |> css
        ]
        [ Locale.string vc.locale msg |> text
        ]

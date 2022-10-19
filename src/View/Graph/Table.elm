module View.Graph.Table exposing (..)

import Api.Data
import Config.View as View
import Css
import Css.Table
import FontAwesome
import Html
import Html.Attributes as Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Currency as Currency
import Model.Graph.Table as T
import Msg.Graph exposing (Msg(..))
import RecordSetter exposing (..)
import Table
import Tuple exposing (..)
import Util.View exposing (loadingSpinner, none)
import View.Locale as Locale


table : View.Config -> List (Attribute msg) -> Maybe (Maybe String -> msg) -> Maybe Float -> Table.Config data msg -> T.Table data -> Html msg
table vc attributes filterMsg height config tbl =
    let
        minHeight =
            tbl.data
                |> List.length
                |> toFloat
                |> (+) 2
                |> (*) vc.theme.table.rowHeight
                |> Basics.min vc.theme.table.maxHeight
    in
    div
        [ Css.Table.root vc |> css
        ]
        [ div
            ([ (height
                    |> Maybe.withDefault vc.theme.table.maxHeight
                    |> Basics.max minHeight
                    |> Css.px
                    |> Css.maxHeight
               )
                :: (minHeight
                        |> Css.px
                        |> Css.height
                   )
                :: Css.Table.tableRoot vc
                |> css
             ]
                ++ attributes
            )
            ((Maybe.map2
                (\filter fm ->
                    div
                        [ Css.Table.filter vc |> css
                        ]
                        [ input
                            [ Css.Table.filterInput vc |> css
                            , type_ "text"
                            , onInput (Just >> fm)
                            , id "tableFilter"
                            , autocomplete False
                            , spellcheck False
                            ]
                            []
                        ]
                )
                tbl.filter
                filterMsg
                |> Maybe.withDefault Util.View.none
             )
                :: Table.view config tbl.state tbl.filtered
                :: (if tbl.loading then
                        [ loadingSpinner vc Css.Table.loadingSpinner
                        ]

                    else if List.isEmpty tbl.data then
                        [ div
                            [ Css.Table.emptyHint vc |> css
                            ]
                            [ Locale.string vc.locale "This table is empty" |> text
                            ]
                        ]

                    else
                        []
                   )
            )
        , Maybe.map
            (\fm ->
                div
                    [ Css.Table.sidebar vc |> css
                    , onClick
                        (fm
                            (if tbl.filter == Nothing then
                                Just ""

                             else
                                Nothing
                            )
                        )
                    ]
                    [ FontAwesome.icon FontAwesome.search
                        |> Html.Styled.fromUnstyled
                    ]
            )
            filterMsg
            |> Maybe.withDefault Util.View.none
        ]


customizations : View.Config -> Table.Customizations data msg
customizations vc =
    Table.defaultCustomizations
        |> s_tableAttrs [ Css.Table.table vc |> css ]
        |> s_thead (simpleThead vc)
        |> s_rowAttrs (\_ -> [ Css.Table.row vc |> css ])


simpleThead : View.Config -> List ( String, Table.Status, Attribute msg ) -> Table.HtmlDetails msg
simpleThead vc headers =
    Table.HtmlDetails [ Css.Table.headRow vc |> css ] (List.map (simpleTheadHelp vc) headers)


simpleTheadHelp : View.Config -> ( String, Table.Status, Attribute msg ) -> Html msg
simpleTheadHelp vc ( name, status, click ) =
    let
        n =
            Locale.string vc.locale name
                |> Html.text

        withCss =
            pair (List.map (\( a, b ) -> Html.style a b) (Css.Table.headCellSortable vc))

        ( attr, content ) =
            case status of
                Table.Unsortable ->
                    ( [], [ n ] )

                Table.Sortable selected ->
                    [ n
                    , Html.text " "
                    , if selected then
                        FontAwesome.sortUp
                            |> FontAwesome.icon

                      else
                        FontAwesome.sortDown
                            |> FontAwesome.icon
                    ]
                        |> withCss

                Table.Reversible Nothing ->
                    [ n
                    , Html.text " "
                    , FontAwesome.sort
                        |> FontAwesome.icon
                    ]
                        |> withCss

                Table.Reversible (Just isReversed) ->
                    [ n
                    , Html.text " "
                    , if isReversed then
                        FontAwesome.sortUp
                            |> FontAwesome.icon

                      else
                        FontAwesome.sortDown
                            |> FontAwesome.icon
                    ]
                        |> withCss
    in
    Html.div attr content
        |> Html.Styled.fromUnstyled
        |> List.singleton
        |> th
            [ click
            , Css.Table.headCell vc |> css
            ]


htmlColumn : View.Config -> String -> (data -> comparable) -> (data -> List (Html msg)) -> Table.Column data msg
htmlColumn vc name accessor html =
    Table.veryCustomColumn
        { name = name
        , viewData = html >> Table.HtmlDetails [ Css.Table.cell vc |> css ]
        , sorter = Table.increasingOrDecreasingBy accessor
        }


stringColumn : View.Config -> String -> (data -> String) -> Table.Column data msg
stringColumn vc name accessor =
    Table.veryCustomColumn
        { name = name
        , viewData = accessor >> text >> List.singleton >> Table.HtmlDetails [ Css.Table.cell vc |> css ]
        , sorter = Table.increasingOrDecreasingBy accessor
        }


timestampColumn : View.Config -> String -> (data -> Int) -> Table.Column data msg
timestampColumn vc name accessor =
    Table.veryCustomColumn
        { name = name
        , viewData = accessor >> Locale.timestamp vc.locale >> text >> List.singleton >> Table.HtmlDetails [ Css.Table.cell vc |> css ]
        , sorter = Table.increasingOrDecreasingBy accessor
        }


intColumn : View.Config -> String -> (data -> Int) -> Table.Column data msg
intColumn vc name accessor =
    Table.veryCustomColumn
        { name = name
        , viewData = accessor >> Locale.int vc.locale >> text >> List.singleton >> Table.HtmlDetails [ Css.Table.numberCell vc |> css ]
        , sorter = Table.increasingOrDecreasingBy accessor
        }


valueColumn : View.Config -> String -> String -> (data -> Api.Data.Values) -> Table.Column data msg
valueColumn vc coinCode name getValues =
    Table.veryCustomColumn
        { name = name
        , viewData = getValues >> valuesCell vc coinCode
        , sorter = Table.decreasingOrIncreasingBy (getValues >> valuesSorter vc)
        }


valuesCell : View.Config -> String -> Api.Data.Values -> Table.HtmlDetails msg
valuesCell vc coinCode values =
    Locale.currency vc.locale coinCode values
        |> text
        |> List.singleton
        |> Table.HtmlDetails
            [ Currency.valuesToFloat vc.locale.currency values
                |> Maybe.withDefault 0
                |> (>) 0
                |> Css.Table.valuesCell vc
                |> css
            ]


valuesSorter : View.Config -> Api.Data.Values -> Float
valuesSorter vc values =
    Currency.valuesToFloat vc.locale.currency values
        |> Maybe.withDefault 0


tickIf : View.Config -> (a -> Bool) -> a -> Html msg
tickIf vc has a =
    if has a then
        FontAwesome.icon FontAwesome.check
            |> Html.Styled.fromUnstyled
            |> List.singleton
            |> span
                [ Css.Table.tick vc |> css
                ]

    else
        none


tickColumn : View.Config -> String -> (data -> Bool) -> Table.Column data msg
tickColumn vc title accessor =
    htmlColumn vc
        title
        (\data ->
            if accessor data then
                "Y"

            else
                "N"
        )
        (\data ->
            if accessor data then
                FontAwesome.icon FontAwesome.check
                    |> Html.Styled.fromUnstyled
                    |> List.singleton

            else
                []
        )

module View.Graph.Table exposing (Tools, addressColumn, csvTool, customizations, filterTool, htmlColumn, htmlColumnWithSorter, info, intColumn, intColumnWithoutValueDetailFormatting, maybeIntColumn, noTools, simpleThead, simpleTheadHelp, stringColumn, table, tableHint, tickColumn, tickIf, timestampColumn, valueAndTokensColumnWithOptions, valueColumn, valueColumnWithOptions, valueColumnWithoutCode, valuesCell, valuesCss, valuesSorter)

import Api.Data
import Config.View as View
import Css
import Css.Table exposing (Styles)
import Dict exposing (Dict)
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Currency exposing (AssetIdentifier, asset, assetFromBase)
import Model.Graph.Table as T
import RecordSetter exposing (..)
import Table
import Tuple exposing (..)
import Tuple3
import Util.View exposing (copyableLongIdentifier, loadingSpinner, none)
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


table : Styles -> View.Config -> List (Attribute msg) -> Tools msg -> Table.Config data msg -> T.Table data -> Html msg
table styles vc attributes tools config tbl =
    div
        [ styles.root vc |> css
        ]
        [ div
            ((styles.tableRoot vc |> css) :: attributes)
            ((Maybe.map2
                (\term fm ->
                    div
                        [ styles.filter vc |> css
                        ]
                        [ input
                            [ styles.filterInput vc |> css
                            , type_ "text"
                            , onInput (Just >> fm)
                            , id "tableFilter"
                            , autocomplete False
                            , spellcheck False
                            , value term
                            ]
                            []
                        ]
                )
                tbl.searchTerm
                tools.filter
                |> Maybe.withDefault Util.View.none
             )
                :: Table.view config tbl.state tbl.filtered
                :: (if tbl.loading then
                        [ loadingSpinner vc styles.loadingSpinner
                        ]

                    else if List.isEmpty tbl.data then
                        [ tableHint styles vc "This table is empty"
                        ]

                    else if List.isEmpty tbl.filtered then
                        [ tableHint styles vc "No rows match your filter criteria"
                        ]

                    else
                        []
                   )
            )
        , if tools == noTools then
            none

          else
            [ Maybe.map (filterTool styles vc tbl) tools.filter
            , Maybe.map (csvTool styles vc) tools.csv
            ]
                |> List.filterMap identity
                |> div
                    [ styles.sidebar vc |> css
                    ]
        ]


filterTool : Styles -> View.Config -> T.Table data -> (Maybe String -> msg) -> Html msg
filterTool styles vc tbl filterMsg =
    let
        isInactive =
            tbl.searchTerm == Nothing
    in
    FontAwesome.icon FontAwesome.search
        |> Html.Styled.fromUnstyled
        |> List.singleton
        |> div
            [ onClick
                (filterMsg
                    (if isInactive then
                        Just ""

                     else
                        Nothing
                    )
                )
            , not isInactive |> styles.sidebarIcon vc |> css
            , Locale.string vc.locale "Filter table" |> title
            ]


csvTool : Styles -> View.Config -> msg -> Html msg
csvTool styles vc msg =
    FontAwesome.icon FontAwesome.download
        |> Html.Styled.fromUnstyled
        |> List.singleton
        |> div
            [ onClick msg
            , styles.sidebarIcon vc False |> css
            , Locale.string vc.locale "Download table as CSV" |> title
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


addressColumn : Styles -> View.Config -> String -> (data -> String) -> (data -> msg) -> Table.Column data msg
addressColumn styles vc name accessor onCli =
    Table.veryCustomColumn
        { name = name
        , viewData =
            \data ->
                accessor data
                    |> copyableLongIdentifier vc
                        [ onClick (onCli data)
                        , Css.cursor Css.pointer
                            |> List.singleton
                            |> css
                        ]
                    |> List.singleton
                    |> Table.HtmlDetails [ styles.cell vc |> css ]
        , sorter = Table.increasingOrDecreasingBy accessor
        }


timestampColumn : Styles -> View.Config -> String -> (data -> Int) -> Table.Column data msg
timestampColumn styles vc name accessor =
    Table.veryCustomColumn
        { name = name
        , viewData = accessor >> Locale.timestamp vc.locale >> text >> List.singleton >> Table.HtmlDetails [ styles.cell vc |> css ]
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


intColumnWithoutValueDetailFormatting : Styles -> View.Config -> String -> (data -> Int) -> Table.Column data msg
intColumnWithoutValueDetailFormatting styles vc name accessor =
    Table.veryCustomColumn
        { name = name
        , viewData =
            accessor
                >> Locale.intWithoutValueDetailFormatting vc.locale
                >> text
                >> List.singleton
                >> Table.HtmlDetails [ styles.numberCell vc |> css ]
        , sorter = Table.increasingOrDecreasingBy accessor
        }


maybeIntColumn : Styles -> View.Config -> String -> (data -> Maybe Int) -> Table.Column data msg
maybeIntColumn styles vc name accessor =
    Table.veryCustomColumn
        { name = name
        , viewData =
            accessor
                >> Maybe.map (Locale.intWithoutValueDetailFormatting vc.locale)
                >> Maybe.withDefault ""
                >> text
                >> List.singleton
                >> Table.HtmlDetails [ styles.numberCell vc |> css ]
        , sorter = Table.increasingOrDecreasingBy (accessor >> Maybe.withDefault 0)
        }


valueColumn : Styles -> View.Config -> (data -> AssetIdentifier) -> String -> (data -> Api.Data.Values) -> Table.Column data msg
valueColumn styles =
    valueColumnWithOptions styles False


valueColumnWithoutCode : Styles -> View.Config -> (data -> AssetIdentifier) -> String -> (data -> Api.Data.Values) -> Table.Column data msg
valueColumnWithoutCode styles =
    valueColumnWithOptions styles True


valueColumnWithOptions : Styles -> Bool -> View.Config -> (data -> AssetIdentifier) -> String -> (data -> Api.Data.Values) -> Table.Column data msg
valueColumnWithOptions styles hideCode vc getCoinCode name getValues =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> getValues data |> valuesCell styles vc hideCode (getCoinCode data)
        , sorter = Table.decreasingOrIncreasingBy (\data -> getValues data |> valuesSorter vc (getCoinCode data))
        }


valueAndTokensColumnWithOptions : Styles -> Bool -> View.Config -> (data -> String) -> String -> (data -> Api.Data.Values) -> (data -> Maybe (Dict String Api.Data.Values)) -> Table.Column data msg
valueAndTokensColumnWithOptions styles _ vc getCoinCode name getValues getTokens =
    let
        assets data =
            ( assetFromBase (getCoinCode data), getValues data )
                :: (getTokens data |> Maybe.map (Dict.toList >> List.map (\( k, v ) -> ( asset (getCoinCode data) k, v ))) |> Maybe.withDefault [])
    in
    Table.veryCustomColumn
        { name = name
        , viewData =
            \data ->
                assets data
                    |> Locale.currency vc.locale
                    |> text
                    |> List.singleton
                    |> Table.HtmlDetails
                        [ styles.valuesCell vc False |> css
                        ]
        , sorter = Table.decreasingOrIncreasingBy (assets >> Locale.currencyAsFloat vc.locale)
        }


valuesCell : Styles -> View.Config -> Bool -> AssetIdentifier -> Api.Data.Values -> Table.HtmlDetails msg
valuesCell styles vc hideCode coinCode values =
    (if hideCode then
        Locale.currencyWithoutCode

     else
        Locale.currency
    )
        vc.locale
        [ ( coinCode, values ) ]
        |> text
        |> List.singleton
        |> Table.HtmlDetails
            [ valuesCss styles vc coinCode values |> css
            ]


valuesCss : Styles -> View.Config -> AssetIdentifier -> Api.Data.Values -> List Css.Style
valuesCss styles vc asset values =
    Locale.valuesToFloat vc.locale asset values
        |> Maybe.withDefault 0
        |> (>) 0
        |> styles.valuesCell vc


valuesSorter : View.Config -> AssetIdentifier -> Api.Data.Values -> Float
valuesSorter vc asset values =
    Locale.valuesToFloat vc.locale asset values
        |> Maybe.withDefault 0


tickIf : Styles -> View.Config -> (a -> Bool) -> a -> Html msg
tickIf styles vc has a =
    if has a then
        FontAwesome.icon FontAwesome.check
            |> Html.Styled.fromUnstyled
            |> List.singleton
            |> span
                [ styles.tick vc |> css
                ]

    else
        none


tickColumn : Styles -> View.Config -> String -> (data -> Bool) -> Table.Column data msg
tickColumn styles vc title accessor =
    htmlColumn styles
        vc
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


info : Styles -> View.Config -> T.Table data -> Html msg
info styles vc { data, filtered } =
    let
        ld =
            List.length data

        lf =
            List.length filtered
    in
    div
        [ styles.info vc |> css
        ]
        [ text <|
            if ld /= lf then
                Locale.interpolated vc.locale
                    "Showing {0} of {1} items"
                    [ String.fromInt lf, String.fromInt ld ]

            else
                Locale.interpolated vc.locale
                    "{0} items"
                    [ String.fromInt lf ]
        ]


tableHint : Styles -> View.Config -> String -> Html msg
tableHint styles vc msg =
    div
        [ styles.emptyHint vc |> css
        ]
        [ Locale.string vc.locale msg |> text
        ]

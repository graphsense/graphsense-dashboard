module View.Graph.Table exposing (..)

import Api.Data
import Config.Graph as Graph
import Config.View as View
import Css
import Css.Table
import Dict exposing (Dict)
import FontAwesome
import Html
import Html.Attributes as Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Currency as Currency exposing (AssetIdentifier, asset, assetFromBase)
import Model.Graph.Table as T
import RecordSetter exposing (..)
import Table
import Tuple exposing (..)
import Util.View exposing (copyableLongIdentifier, loadingSpinner, none)
import View.Graph.Label as Label
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


table : View.Config -> List (Attribute msg) -> Tools msg -> Table.Config data msg -> T.Table data -> Html msg
table vc attributes tools config tbl =
    div
        [ Css.Table.root vc |> css
        ]
        [ div
            ([ Css.Table.tableRoot vc
                |> css
             ]
                ++ attributes
            )
            ((Maybe.map2
                (\term fm ->
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
                        [ loadingSpinner vc Css.Table.loadingSpinner
                        ]

                    else if List.isEmpty tbl.data then
                        [ tableHint vc "This table is empty"
                        ]

                    else if List.isEmpty tbl.filtered then
                        [ tableHint vc "No rows match your filter criteria"
                        ]

                    else
                        []
                   )
            )
        , if tools == noTools then
            none

          else
            [ Maybe.map (filterTool vc tbl) tools.filter
            , Maybe.map (csvTool vc) tools.csv
            ]
                |> List.filterMap identity
                |> div
                    [ Css.Table.sidebar vc |> css
                    ]
        ]


filterTool : View.Config -> T.Table data -> (Maybe String -> msg) -> Html msg
filterTool vc tbl filterMsg =
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
            , not isInactive |> Css.Table.sidebarIcon vc |> css
            , Locale.string vc.locale "Filter table" |> title
            ]


csvTool : View.Config -> msg -> Html msg
csvTool vc msg =
    FontAwesome.icon FontAwesome.download
        |> Html.Styled.fromUnstyled
        |> List.singleton
        |> div
            [ onClick msg
            , Css.Table.sidebarIcon vc False |> css
            , Locale.string vc.locale "Download table as CSV" |> title
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
    htmlColumnWithSorter (Table.increasingOrDecreasingBy accessor) vc name accessor html


htmlColumnWithSorter : Table.Sorter data -> View.Config -> String -> (data -> comparable) -> (data -> List (Html msg)) -> Table.Column data msg
htmlColumnWithSorter sorter vc name accessor html =
    Table.veryCustomColumn
        { name = name
        , viewData = html >> Table.HtmlDetails [ Css.Table.cell vc |> css ]
        , sorter = sorter
        }


stringColumn : View.Config -> String -> (data -> String) -> Table.Column data msg
stringColumn vc name accessor =
    Table.veryCustomColumn
        { name = name
        , viewData = accessor >> text >> List.singleton >> Table.HtmlDetails [ Css.Table.cell vc |> css ]
        , sorter = Table.increasingOrDecreasingBy accessor
        }


addressColumn : View.Config -> String -> (data -> String) -> (data -> msg) -> Table.Column data msg
addressColumn vc name accessor onCli =
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
                    |> Table.HtmlDetails [ Css.Table.cell vc |> css ]
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
        , viewData =
            accessor
                >> Locale.int vc.locale
                >> text
                >> List.singleton
                >> Table.HtmlDetails [ Css.Table.numberCell vc |> css ]
        , sorter = Table.increasingOrDecreasingBy accessor
        }


intColumnWithoutValueDetailFormatting : View.Config -> String -> (data -> Int) -> Table.Column data msg
intColumnWithoutValueDetailFormatting vc name accessor =
    Table.veryCustomColumn
        { name = name
        , viewData =
            accessor
                >> Locale.intWithoutValueDetailFormatting vc.locale
                >> text
                >> List.singleton
                >> Table.HtmlDetails [ Css.Table.numberCell vc |> css ]
        , sorter = Table.increasingOrDecreasingBy accessor
        }


maybeIntColumn : View.Config -> String -> (data -> Maybe Int) -> Table.Column data msg
maybeIntColumn vc name accessor =
    Table.veryCustomColumn
        { name = name
        , viewData =
            accessor
                >> Maybe.map (Locale.intWithoutValueDetailFormatting vc.locale)
                >> Maybe.withDefault ""
                >> text
                >> List.singleton
                >> Table.HtmlDetails [ Css.Table.numberCell vc |> css ]
        , sorter = Table.increasingOrDecreasingBy (accessor >> Maybe.withDefault 0)
        }


valueColumn : View.Config -> (data -> AssetIdentifier) -> String -> (data -> Api.Data.Values) -> Table.Column data msg
valueColumn =
    valueColumnWithOptions False


valueColumnWithoutCode : View.Config -> (data -> AssetIdentifier) -> String -> (data -> Api.Data.Values) -> Table.Column data msg
valueColumnWithoutCode =
    valueColumnWithOptions True


valueColumnWithOptions : Bool -> View.Config -> (data -> AssetIdentifier) -> String -> (data -> Api.Data.Values) -> Table.Column data msg
valueColumnWithOptions hideCode vc getCoinCode name getValues =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> getValues data |> valuesCell vc hideCode (getCoinCode data)
        , sorter = Table.decreasingOrIncreasingBy (\data -> getValues data |> valuesSorter vc (getCoinCode data))
        }


valueAndTokensColumnWithOptions : Bool -> View.Config -> (data -> String) -> String -> (data -> Api.Data.Values) -> (data -> Maybe (Dict String Api.Data.Values)) -> Table.Column data msg
valueAndTokensColumnWithOptions hideCode vc getCoinCode name getValues getTokens =
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
                        [ Css.Table.valuesCell vc False |> css
                        ]
        , sorter = Table.decreasingOrIncreasingBy (assets >> Locale.currencyAsFloat vc.locale)
        }


valuesCell : View.Config -> Bool -> AssetIdentifier -> Api.Data.Values -> Table.HtmlDetails msg
valuesCell vc hideCode coinCode values =
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
            [ valuesCss vc coinCode values |> css
            ]


valuesCss : View.Config -> AssetIdentifier -> Api.Data.Values -> List Css.Style
valuesCss vc asset values =
    Locale.valuesToFloat vc.locale asset values
        |> Maybe.withDefault 0
        |> (>) 0
        |> Css.Table.valuesCell vc


valuesSorter : View.Config -> AssetIdentifier -> Api.Data.Values -> Float
valuesSorter vc asset values =
    Locale.valuesToFloat vc.locale asset values
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


info : View.Config -> T.Table data -> Html msg
info vc { data, filtered } =
    let
        ld =
            List.length data

        lf =
            List.length filtered
    in
    div
        [ Css.Table.info vc |> css
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


tableHint : View.Config -> String -> Html msg
tableHint vc msg =
    div
        [ Css.Table.emptyHint vc |> css
        ]
        [ Locale.string vc.locale msg |> text
        ]

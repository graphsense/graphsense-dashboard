module View.Graph.Table exposing (..)

import Api.Data
import Config.View as View
import Css.Table
import FontAwesome
import Html
import Html.Attributes as Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model.Currency as Currency
import Msg.Graph exposing (Msg(..))
import RecordSetter exposing (..)
import Table
import Tuple exposing (..)
import View.Locale as Locale


table : View.Config -> Table.Config data Msg -> Table.State -> List data -> Html Msg
table vc config state data =
    div
        [ Css.Table.root vc |> css
        ]
        [ Table.view config state data
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
                    , FontAwesome.sort
                        |> FontAwesome.icon
                    ]
                        |> withCss

                Table.Reversible (Just isReversed) ->
                    [ n
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
            [ click ]


stringColumn : View.Config -> String -> (data -> String) -> Table.Column data Msg
stringColumn vc name accessor =
    Table.veryCustomColumn
        { name = name
        , viewData = accessor >> text >> List.singleton >> Table.HtmlDetails [ Css.Table.cell vc |> css ]
        , sorter = Table.increasingOrDecreasingBy accessor
        }


valueColumn : View.Config -> String -> String -> (data -> Api.Data.Values) -> Table.Column data Msg
valueColumn vc coinCode name getValues =
    Table.veryCustomColumn
        { name = name
        , viewData = getValues >> valuesCell vc coinCode
        , sorter = Table.decreasingOrIncreasingBy (getValues >> valuesSorter vc)
        }


valuesCell : View.Config -> String -> Api.Data.Values -> Table.HtmlDetails Msg
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

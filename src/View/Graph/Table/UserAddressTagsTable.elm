module View.Graph.Table.UserAddressTagsTable exposing (config, prepareCSV)

import Config.Graph as Graph
import Config.View as View
import Css
import Css.Table exposing (styles)
import Css.View
import Dict
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Init.Graph.Table
import Model.Graph.Table exposing (Table, titleAddress, titleCurrency, titleLabel)
import Model.Graph.Table.UserAddressTagsTable exposing (titleAbuse, titleCategory, titleDefinesEntity, titleSource)
import Model.Graph.Tag as Tag
import Msg.Graph exposing (Msg(..))
import RecordSetter exposing (..)
import Table
import Util.Csv
import Util.Graph
import Util.View
import View.Graph.Table as T exposing (customizations)


config : View.Config -> Graph.Config -> Table.Config Tag.UserTag Msg
config vc gc =
    let
        toMsg data =
            UserClickedAddressInTable
                { currency = data.currency
                , address = data.address
                }
    in
    Table.customConfig
        { toId = \data -> data.currency ++ data.address ++ data.label
        , toMsg = TableNewState
        , columns =
            [ toMsg
                |> T.addressColumn styles vc titleAddress .address
            , T.stringColumn styles vc titleCurrency (.currency >> String.toUpper)
            , T.stringColumn styles vc titleLabel .label
            , T.tickColumn styles vc titleDefinesEntity .isClusterDefiner
            , T.htmlColumn styles
                vc
                titleSource
                .source
                (\data ->
                    let
                        source =
                            data.source

                        truncated =
                            if String.length source > vc.theme.table.urlMaxLength then
                                Util.View.truncate vc.theme.table.urlMaxLength source

                            else
                                source
                    in
                    [ if String.startsWith "http" source then
                        text truncated
                            |> List.singleton
                            |> a
                                [ href source
                                , target "_blank"
                                , Css.View.link vc |> css
                                ]

                      else
                        text truncated
                    ]
                )
            , T.stringColumn styles vc titleCategory (.category >> Util.Graph.getCategory gc >> Maybe.withDefault "")
            , T.stringColumn styles vc titleAbuse (.abuse >> Util.Graph.getAbuse gc >> Maybe.withDefault "")
            ]
        , customizations =
            customizations styles vc
                |> s_rowAttrs
                    (\data ->
                        [ Css.Table.row vc
                            ++ (if data.isClusterDefiner then
                                    data.category
                                        |> Maybe.map
                                            (vc.theme.graph.categoryToColor
                                                >> Util.View.setAlpha 0.7
                                                >> Util.View.toCssColor
                                                >> Css.backgroundColor
                                                >> Css.important
                                                >> List.singleton
                                            )
                                        |> Maybe.withDefault []

                                else
                                    []
                               )
                            |> css
                        ]
                    )
        }


n : x -> ( x, List y )
n s =
    ( s, [] )


prepareCSV : Graph.Config -> Tag.UserTag -> List ( ( String, List String ), String )
prepareCSV gc row =
    [ ( n "address", Util.Csv.string row.address )
    , ( n "currency", Util.Csv.string <| String.toUpper row.currency )
    , ( n "label", Util.Csv.string row.label )
    , ( n "is_cluster_definer", Util.Csv.bool row.isClusterDefiner )
    , ( n "source", Util.Csv.string row.source )
    , ( n "category", row.category |> Util.Graph.getCategory gc |> Maybe.withDefault "" |> Util.Csv.string )
    , ( n "abuse", row.abuse |> Util.Graph.getAbuse gc |> Maybe.withDefault "" |> Util.Csv.string )
    ]

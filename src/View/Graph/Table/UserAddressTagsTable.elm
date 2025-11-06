module View.Graph.Table.UserAddressTagsTable exposing (config, prepareCSV)

import Config.Update
import Config.View as View
import Css
import Css.Table exposing (styles)
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model.Graph.Table exposing (titleAddress, titleCurrency, titleLabel)
import Model.Graph.Table.UserAddressTagsTable exposing (titleAbuse, titleCategory, titleDefinesEntity, titleSource)
import Model.Graph.Tag as Tag
import Msg.Graph exposing (Msg(..))
import RecordSetter exposing (..)
import Table
import Util.Csv
import Util.View
import View.Graph.Table as T exposing (customizations)


config : View.Config -> Table.Config Tag.UserTag Msg
config vc =
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
            , T.stringColumn styles
                vc
                titleCategory
                (.category >> Maybe.andThen (View.getConceptName vc) >> Maybe.withDefault "")
            , T.stringColumn styles vc titleAbuse (.abuse >> View.getAbuseName vc >> Maybe.withDefault "")
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


prepareCSV : Config.Update.Config -> Tag.UserTag -> List ( String, String )
prepareCSV uc row =
    [ ( "address", Util.Csv.string row.address )
    , ( "currency", Util.Csv.string <| String.toUpper row.currency )
    , ( "label", Util.Csv.string row.label )
    , ( "is_cluster_definer", Util.Csv.bool row.isClusterDefiner )
    , ( "source", Util.Csv.string row.source )
    , ( "category", row.category |> Maybe.andThen (View.getConceptName uc) |> Maybe.withDefault "" |> Util.Csv.string )
    , ( "abuse", row.abuse |> View.getAbuseName uc |> Maybe.withDefault "" |> Util.Csv.string )
    ]

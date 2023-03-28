module View.Graph.Table.UserAddressTagsTable exposing (..)

import Api.Data
import Config.Graph as Graph
import Config.View as View
import Css
import Css.Table
import Css.View
import Dict
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Init.Graph.Table
import Model.Graph.Table as T exposing (Table)
import Model.Graph.Tag as Tag
import Msg.Graph exposing (Msg(..))
import RecordSetter exposing (..)
import Table
import Util.Csv
import Util.Graph
import Util.View exposing (truncate)
import View.Graph.Table as T exposing (customizations, valueColumn)
import View.Locale as Locale


init : Table Tag.UserTag
init =
    Init.Graph.Table.initSorted True filter "Label"
        |> s_loading False


filter : String -> Tag.UserTag -> Bool
filter f a =
    String.contains f a.address
        || String.contains f a.label


titleAddress : String
titleAddress =
    "Address"


titleCurrency : String
titleCurrency =
    "Currency"


titleLabel : String
titleLabel =
    "Label"


titleDefinesEntity : String
titleDefinesEntity =
    "Defines entity"


titleSource : String
titleSource =
    "Source"


titleCategory : String
titleCategory =
    "Category"


titleAbuse : String
titleAbuse =
    "Abuse"


config : View.Config -> Graph.Config -> Table.Config Tag.UserTag Msg
config vc gc =
    Table.customConfig
        { toId = \data -> data.currency ++ data.address ++ data.label
        , toMsg = TableNewState
        , columns =
            [ T.addressColumn vc titleAddress .address (\v -> UserClickedCopyToClipboard v)
            , T.stringColumn vc titleCurrency (.currency >> String.toUpper)
            , T.stringColumn vc titleLabel .label
            , T.tickColumn vc titleDefinesEntity .isClusterDefiner
            , T.htmlColumn vc
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
            , T.stringColumn vc titleCategory (.category >> Util.Graph.getCategory gc >> Maybe.withDefault "")
            , T.stringColumn vc titleAbuse (.abuse >> Util.Graph.getAbuse gc >> Maybe.withDefault "")
            ]
        , customizations =
            customizations vc
                |> s_rowAttrs
                    (\data ->
                        [ Css.Table.row vc
                            ++ (if data.isClusterDefiner then
                                    data.category
                                        |> Maybe.andThen
                                            (\category ->
                                                Dict.get category gc.colors
                                                    |> Maybe.map
                                                        (Util.View.setAlpha 0.7
                                                            >> Util.View.toCssColor
                                                            >> Css.backgroundColor
                                                            >> Css.important
                                                            >> List.singleton
                                                        )
                                            )
                                        |> Maybe.withDefault []

                                else
                                    []
                               )
                            |> css
                        ]
                    )
        }


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

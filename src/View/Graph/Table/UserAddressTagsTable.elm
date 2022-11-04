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


config : View.Config -> Graph.Config -> Table.Config Tag.UserTag Msg
config vc gc =
    Table.customConfig
        { toId = \data -> data.currency ++ data.address ++ data.label
        , toMsg = TableNewState
        , columns =
            [ T.stringColumn vc "Address" .address
            , T.stringColumn vc "Currency" (.currency >> String.toUpper)
            , T.stringColumn vc "Label" .label
            , T.tickColumn vc "Defines entity" .isClusterDefiner
            , T.htmlColumn vc
                "Source"
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
            , T.stringColumn vc "Category" (.category >> Maybe.withDefault "")
            , T.stringColumn vc "Abuse" (.abuse >> Maybe.withDefault "")
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


prepareCSV : Tag.UserTag -> List ( String, String )
prepareCSV row =
    Debug.todo "prepareCSV"

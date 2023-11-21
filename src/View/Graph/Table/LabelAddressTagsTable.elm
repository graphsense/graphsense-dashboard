module View.Graph.Table.LabelAddressTagsTable exposing (..)

import Api.Data
import Config.View as View
import Css
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Graph.Table
import Model.Graph.Table exposing (Table)
import Msg.Graph exposing (Msg(..))
import Table
import Util.View exposing (copyableLongIdentifier)
import View.Graph.Table as T exposing (customizations)


init : Table Api.Data.AddressTag
init =
    Init.Graph.Table.initSorted True "Confidence"


filter : String -> Api.Data.AddressTag -> Bool
filter f a =
    String.contains f a.address
        || String.contains f a.label


config : View.Config -> Table.Config Api.Data.AddressTag Msg
config vc =
    Table.customConfig
        { toId = \data -> data.currency ++ data.address ++ data.label
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn vc
                "Address"
                .address
                (\data ->
                    [ copyableLongIdentifier vc
                        [ UserClickedAddressInTable
                            { address = data.address
                            , currency = String.toLower data.currency
                            }
                            |> onClick
                        , css [ Css.cursor Css.pointer ]
                        ]
                        data.address
                    ]
                )
            , T.stringColumn vc "Entity" (.entity >> String.fromInt)
            , T.stringColumn vc "Currency" (.currency >> String.toUpper)
            , T.stringColumn vc "Label" .label
            , T.htmlColumn vc
                "Source"
                (.source >> Maybe.withDefault "")
                (\data ->
                    let
                        source =
                            data.source
                                |> Maybe.withDefault ""

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
            , T.stringColumn vc "Actor id" (.actor >> Maybe.withDefault "")
            , T.stringColumn vc
                "Confidence"
                (.confidence >> Maybe.withDefault "")
            , T.htmlColumn vc
                "TagPack"
                .tagpackTitle
                (\data ->
                    let
                        uri =
                            data.tagpackUri |> Maybe.withDefault ""
                    in
                    [ if String.startsWith "http" uri then
                        text data.tagpackTitle
                            |> List.singleton
                            |> a
                                [ href uri
                                , target "_blank"
                                , Css.View.link vc |> css
                                ]

                      else
                        text data.tagpackTitle
                    ]
                )
            , T.stringColumn vc "Creator" .tagpackCreator
            ]
        , customizations = customizations vc
        }

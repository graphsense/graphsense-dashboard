module View.Graph.Table.LabelAddressTagsTable exposing (config)

import Api.Data
import Config.View as View
import Css
import Css.Table exposing (styles)
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Graph.Table
import Model.Graph.Table exposing (Table)
import Model.Graph.Table.LabelAddressTagsTable exposing (titleConfidence)
import Msg.Graph exposing (Msg(..))
import Table
import Util.View exposing (copyableLongIdentifier)
import View.Graph.Table as T exposing (customizations)


config : View.Config -> Table.Config Api.Data.AddressTag Msg
config vc =
    Table.customConfig
        { toId = \data -> data.currency ++ data.address ++ data.label
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn styles
                vc
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
            , T.stringColumn styles vc "Entity" (.entity >> String.fromInt)
            , T.stringColumn styles vc "Currency" (.currency >> String.toUpper)
            , T.stringColumn styles vc "Label" .label
            , T.htmlColumn styles
                vc
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
            , T.stringColumn styles vc "Category" (.category >> Maybe.withDefault "")
            , T.stringColumn styles vc "Abuse" (.abuse >> Maybe.withDefault "")
            , T.stringColumn styles vc "Actor id" (.actor >> Maybe.withDefault "")
            , T.stringColumn styles vc titleConfidence (.confidence >> Maybe.withDefault "")
            , T.htmlColumn styles
                vc
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
            , T.stringColumn styles vc "Creator" .tagpackCreator
            ]
        , customizations = customizations styles vc
        }

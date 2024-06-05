module View.Graph.Table.AddressTagsTable exposing (..)

import Api.Data
import Config.Graph as Graph
import Config.View as View
import Css
import Css.Table exposing (styles)
import Css.View
import Dict
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Graph.Table
import Model.Address as A
import Model.Graph.Id as Id
import Model.Graph.Table exposing (Table)
import Model.Graph.Table.AddressTagsTable exposing (..)
import Msg.Graph exposing (Msg(..))
import RecordSetter exposing (..)
import Table
import Util.Graph
import Util.View exposing (copyableLongIdentifier, none)
import View.Graph.Table as T exposing (customizations)
import View.Locale as Locale


config : View.Config -> Graph.Config -> Maybe Api.Data.AddressTag -> Maybe Id.EntityId -> (Id.EntityId -> A.Address -> Bool) -> Table.Config Api.Data.AddressTag Msg
config vc gc bestAddressTag entityId entityHasAddress =
    Table.customConfig
        { toId = \data -> data.currency ++ data.address ++ data.label
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn styles
                vc
                "Address"
                .address
                (\data ->
                    [ entityId
                        |> Maybe.map
                            (\id ->
                                T.tickIf styles
                                    vc
                                    (entityHasAddress id)
                                    { currency = String.toLower data.currency, address = data.address }
                            )
                        |> Maybe.withDefault none
                    , copyableLongIdentifier vc
                        (entityId
                            |> Maybe.map
                                (\id ->
                                    [ UserClickedAddressInEntityTagsTable id data.address
                                        |> onClick
                                    , css [ Css.cursor Css.pointer ]
                                    ]
                                )
                            |> Maybe.withDefault []
                        )
                        data.address
                    ]
                )
            , T.htmlColumn styles
                vc
                "Label"
                .label
                (\{ label, tagpackIsPublic } ->
                    if not tagpackIsPublic && String.isEmpty label then
                        span
                            [ Css.fontStyle Css.italic
                                |> List.singleton
                                |> css
                            ]
                            [ Locale.string vc.locale "proprietary tag"
                                |> text
                            ]
                            |> List.singleton

                    else
                        [ text label ]
                )
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
            , T.stringColumn styles
                vc
                "Category"
                (.category
                    >> Util.Graph.getCategory gc
                    >> Maybe.withDefault ""
                )
            , T.stringColumn styles
                vc
                "Abuse"
                (.abuse
                    >> Util.Graph.getAbuse gc
                    >> Maybe.withDefault ""
                )
            , T.stringColumn styles
                vc
                "Confidence"
                (.confidence >> Maybe.withDefault "")
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
        , customizations =
            customizations styles vc
                |> s_rowAttrs
                    (\data ->
                        [ Css.Table.row vc
                            ++ (bestAddressTag
                                    |> Maybe.andThen
                                        (\tag ->
                                            if tag == data then
                                                tag.category
                                                    |> Maybe.map
                                                        (vc.theme.graph.categoryToColor
                                                            >> Util.View.setAlpha 0.7
                                                            >> Util.View.toCssColor
                                                            >> Css.backgroundColor
                                                            >> Css.important
                                                            >> List.singleton
                                                        )

                                            else
                                                Nothing
                                        )
                                    |> Maybe.withDefault []
                               )
                            |> css
                        ]
                    )
        }

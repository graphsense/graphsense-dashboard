module View.Graph.Table.AddressTagsTable exposing (..)

import Api.Data
import Config.Graph as Graph
import Config.View as View
import Css
import Css.Table
import Css.View
import Dict
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Graph.Table
import List.Extra
import Model.Address as A
import Model.Graph.Id as Id
import Model.Graph.Table as T exposing (Table)
import Msg.Graph exposing (Msg(..))
import RecordSetter exposing (..)
import Table
import Util.View exposing (none, truncate)
import View.Graph.Table as T exposing (customizations, valueColumn)
import View.Locale as Locale


init : Table Api.Data.AddressTag
init =
    Init.Graph.Table.initSorted True filter "Confidence"


filter : String -> Api.Data.AddressTag -> Bool
filter f a =
    String.contains f a.address
        || String.contains f a.label


config : View.Config -> Graph.Config -> Maybe Api.Data.AddressTag -> Maybe Id.EntityId -> (Id.EntityId -> A.Address -> Bool) -> Table.Config Api.Data.AddressTag Msg
config vc gc bestAddressTag entityId entityHasAddress =
    Table.customConfig
        { toId = \data -> data.currency ++ data.address ++ data.label
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn vc
                "Address"
                .address
                (\data ->
                    [ entityId
                        |> Maybe.map
                            (\id ->
                                T.tickIf
                                    (entityHasAddress id)
                                    { currency = String.toLower data.currency, address = data.address }
                            )
                        |> Maybe.withDefault none
                    , span
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
                        [ text data.address
                        ]
                    ]
                )
            , T.stringColumn vc "Entity" (.entity >> String.fromInt)
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
            , T.stringColumn vc
                "Category"
                (.category
                    >> Maybe.andThen (\cat -> List.Extra.find (.id >> (==) cat) gc.entityConcepts)
                    >> Maybe.map .label
                    >> Maybe.withDefault ""
                )
            , T.stringColumn vc
                "Abuse"
                (.abuse
                    >> Maybe.andThen (\cat -> List.Extra.find (.id >> (==) cat) gc.abuseConcepts)
                    >> Maybe.map .label
                    >> Maybe.withDefault ""
                )
            , T.intColumn vc
                "Confidence"
                (.confidenceLevel >> Maybe.withDefault 0)
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
        , customizations =
            customizations vc
                |> s_rowAttrs
                    (\data ->
                        [ Css.Table.row vc
                            ++ (bestAddressTag
                                    |> Maybe.andThen
                                        (\tag ->
                                            if tag == data then
                                                tag.category
                                                    |> Maybe.andThen
                                                        (\category ->
                                                            Dict.get category gc.colors
                                                                |> Maybe.map
                                                                    (Util.View.setAlpha 0.7
                                                                        >> Util.View.toCssColor
                                                                        >> Css.backgroundColor
                                                                        >> List.singleton
                                                                    )
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



{-
   Address

   Creator (tagpack.creator)
-}

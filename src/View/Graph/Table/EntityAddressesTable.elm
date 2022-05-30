module View.Graph.Table.EntityAddressesTable exposing (..)

import Api.Data
import Config.View as View
import Css exposing (cursor, pointer)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Graph.Table
import Model.Graph.Id exposing (EntityId)
import Model.Graph.Table as T exposing (Table)
import Msg.Graph exposing (Msg(..))
import Table
import View.Graph.Table as T exposing (customizations, valueColumn)


init : Table Api.Data.Address
init =
    Init.Graph.Table.init "Address"


config : View.Config -> String -> Maybe EntityId -> Table.Config Api.Data.Address Msg
config vc coinCode entityId =
    Table.customConfig
        { toId = .address
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn vc
                "Address"
                .address
                (\data ->
                    text data.address
                        |> List.singleton
                        |> div
                            (entityId
                                |> Maybe.map
                                    (\id ->
                                        [ UserClickedAddressInEntityAddressesTable id data
                                            |> onClick
                                        , css [ cursor pointer ]
                                        ]
                                    )
                                |> Maybe.withDefault []
                            )
                        |> List.singleton
                )
            , T.timestampColumn vc "First usage" (.firstTx >> .timestamp)
            , T.timestampColumn vc "Last usage" (.lastTx >> .timestamp)
            , T.valueColumn vc coinCode "Final balance" .balance
            , T.valueColumn vc coinCode "Total received" .totalReceived
            ]
        , customizations = customizations vc
        }

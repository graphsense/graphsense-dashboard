module View.Graph.Table.EntityNeighborsTable exposing (..)

import Api.Data
import Config.View as View
import Css exposing (cursor, pointer)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Graph.Table
import Model.Entity as E
import Model.Graph.Id exposing (EntityId)
import Model.Graph.Table exposing (Table)
import Msg.Graph exposing (Msg(..))
import Table
import Util.Csv
import Util.View exposing (none)
import View.Graph.Table as T exposing (customizations, valueColumn)
import View.Graph.Table.AddressNeighborsTable exposing (reduceLabels)
import View.Locale as Locale


columnTitleFromDirection : Bool -> String
columnTitleFromDirection isOutgoing =
    (if isOutgoing then
        "Outgoing"

     else
        "Incoming"
    )
        ++ " entity"


init : Bool -> Table Api.Data.NeighborEntity
init =
    columnTitleFromDirection >> Init.Graph.Table.init filter


filter : String -> Api.Data.NeighborEntity -> Bool
filter f a =
    String.contains f (String.fromInt a.entity.entity)
        || (Maybe.map (List.any (String.contains f)) a.labels |> Maybe.withDefault True)


titleLabels : String
titleLabels =
    "Labels"


titleEntityBalance : String
titleEntityBalance =
    "Entity balance"


titleEntityReceived : String
titleEntityReceived =
    "Entity received"


titleNoAddresses : String
titleNoAddresses =
    "No. addresses"


titleNoTxs : String
titleNoTxs =
    "No. transactions"


titleEstimatedValue : String
titleEstimatedValue =
    "Estimated value"


config : View.Config -> Bool -> String -> Maybe EntityId -> (EntityId -> Bool -> E.Entity -> Bool) -> Table.Config Api.Data.NeighborEntity Msg
config vc isOutgoing coinCode id neighborLayerHasEntity =
    Table.customConfig
        { toId = .entity >> .entity >> String.fromInt
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn vc
                (columnTitleFromDirection isOutgoing)
                (.entity >> .entity >> String.fromInt)
                (\data ->
                    [ id
                        |> Maybe.map
                            (\eid ->
                                T.tickIf vc
                                    (neighborLayerHasEntity eid isOutgoing)
                                    { currency = data.entity.currency
                                    , entity = data.entity.entity
                                    }
                            )
                        |> Maybe.withDefault none
                    , span
                        (id
                            |> Maybe.map
                                (\entityId ->
                                    [ UserClickedEntityInNeighborsTable entityId isOutgoing data
                                        |> onClick
                                    , css [ cursor pointer ]
                                    ]
                                )
                            |> Maybe.withDefault []
                        )
                        [ data.entity.entity
                            |> String.fromInt
                            |> text
                        ]
                    ]
                )
            , T.stringColumn vc titleLabels (.labels >> Maybe.withDefault [] >> reduceLabels)
            , T.valueColumn vc coinCode titleEntityBalance (.entity >> .balance)
            , T.valueColumn vc coinCode titleEntityReceived (.entity >> .totalReceived)
            , T.intColumn vc titleNoAddresses (.entity >> .noAddresses)
            , T.intColumn vc titleNoTxs .noTxs
            , T.valueColumn vc coinCode titleEstimatedValue .value
            ]
        , customizations = customizations vc
        }


n s =
    ( s, [] )


prepareCSV : Bool -> Api.Data.NeighborEntity -> List ( ( String, List String ), String )
prepareCSV isOutgoing row =
    [ ( n <| "entity", Util.Csv.int row.entity.entity )
    , ( n "labels", row.labels |> Maybe.withDefault [] |> String.join ", " |> Util.Csv.string )
    , ( n "no_txs", Util.Csv.int row.noTxs )
    , ( n "no_addresses", Util.Csv.int row.entity.noAddresses )
    ]
        ++ Util.Csv.values "entity_received" row.entity.totalReceived
        ++ Util.Csv.values "entity_balance" row.entity.balance
        ++ Util.Csv.values "estimated_value" row.value
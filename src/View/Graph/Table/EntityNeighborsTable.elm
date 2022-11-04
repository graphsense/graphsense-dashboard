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
            , T.stringColumn vc "Labels" (.labels >> Maybe.withDefault [] >> reduceLabels)
            , T.valueColumn vc coinCode "Entity balance" (.entity >> .balance)
            , T.valueColumn vc coinCode "Entity received" (.entity >> .totalReceived)
            , T.intColumn vc "No. addresses" (.entity >> .noAddresses)
            , T.intColumn vc "No. transactions" .noTxs
            , T.valueColumn vc coinCode "Estimated value" .value
            ]
        , customizations = customizations vc
        }


prepareCSV : Api.Data.NeighborEntity -> List ( String, String )
prepareCSV row =
    Debug.todo "prepareCSV"

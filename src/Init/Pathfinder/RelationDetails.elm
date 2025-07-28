module Init.Pathfinder.RelationDetails exposing (init)

import Api.Data
import Config.Update as Update
import Dict
import Init.Pathfinder.Table.RelationTxsTable as RelationTxsTable
import Maybe.Extra
import Model.Direction exposing (Direction(..))
import Model.Locale as Locale
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.RelationDetails as RelationDetails
import RemoteData


getExposedAssetsForNeighbor : Api.Data.NeighborAddress -> List String
getExposedAssetsForNeighbor data =
    String.toUpper data.address.currency
        :: (data.tokenValues
                |> Maybe.map (Dict.keys >> List.map String.toUpper)
                |> Maybe.withDefault []
           )


init : AggEdge -> RelationDetails.Model
init edge =
    let
        a2bAssets =
            edge.a2b
                |> RemoteData.toMaybe
                |> Maybe.Extra.join
                |> Maybe.map (getExposedAssetsForNeighbor)
                |> Maybe.withDefault []

        b2aAssets =
            edge.b2a
                |> RemoteData.toMaybe
                |> Maybe.Extra.join
                |> Maybe.map (getExposedAssetsForNeighbor)
                |> Maybe.withDefault []
    in
    { a2bTableOpen = False
    , b2aTableOpen = False
    , a2bTable = RelationTxsTable.init Incoming a2bAssets
    , b2aTable = RelationTxsTable.init Outgoing b2aAssets
    , aggEdge = edge
    }

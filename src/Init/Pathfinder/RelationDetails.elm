module Init.Pathfinder.RelationDetails exposing (getExposedAssetsForNeighbor, getExposedAssetsForNeighborWebData, init)

import Api.Data
import Dict
import Init.Pathfinder.Table.RelationTxsTable as RelationTxsTable
import Maybe.Extra
import Model.Direction exposing (Direction(..))
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


getExposedAssetsForNeighborWebData : List String -> RemoteData.WebData (Maybe Api.Data.NeighborAddress) -> List String
getExposedAssetsForNeighborWebData default webData =
    webData
        |> RemoteData.toMaybe
        |> Maybe.Extra.join
        |> Maybe.map getExposedAssetsForNeighbor
        |> Maybe.withDefault default


init : AggEdge -> RelationDetails.Model
init edge =
    let
        -- if data should be missing, fall back to empty asset list
        -- update later assigns data when available
        a2bAssets =
            edge.a2b |> getExposedAssetsForNeighborWebData []

        b2aAssets =
            edge.b2a |> getExposedAssetsForNeighborWebData []
    in
    { a2bTableOpen = False
    , b2aTableOpen = False
    , a2bTable = RelationTxsTable.init Incoming a2bAssets
    , b2aTable = RelationTxsTable.init Outgoing b2aAssets
    , aggEdge = edge
    }

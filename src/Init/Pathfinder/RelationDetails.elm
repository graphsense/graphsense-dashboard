module Init.Pathfinder.RelationDetails exposing (init)

import Init.Pathfinder.Table.RelationTxsTable as RelationTxsTable
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.RelationDetails as RelationDetails


init : AggEdge -> List String -> RelationDetails.Model
init edge assets =
    { a2bTableOpen = False
    , b2aTableOpen = False
    , a2bTable = RelationTxsTable.init Incoming assets
    , b2aTable = RelationTxsTable.init Outgoing assets
    , aggEdge = edge
    }

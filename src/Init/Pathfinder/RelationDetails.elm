module Init.Pathfinder.RelationDetails exposing (init)

import Init.Pathfinder.Table.RelationTxsTable as RelationTxsTable
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.RelationDetails as RelationDetails


init : AggEdge -> RelationDetails.Model
init edge =
    { a2bTableOpen = False
    , b2aTableOpen = False
    , a2bTable = RelationTxsTable.init []
    , b2aTable = RelationTxsTable.init []
    , aggEdge = edge
    }

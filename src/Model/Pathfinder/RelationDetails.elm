module Model.Pathfinder.RelationDetails exposing (Model)

import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.Table.RelationTxsTable as RelationTxsTable


type alias Model =
    { a2bTableOpen : Bool
    , b2aTableOpen : Bool
    , a2bTable : RelationTxsTable.Model
    , b2aTable : RelationTxsTable.Model
    , aggEdge : AggEdge
    }

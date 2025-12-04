module Model.Pathfinder.RelationDetails exposing (Model)

import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.Table.RelationTxsTable as RelationTxsTable
import Msg.Pathfinder.RelationDetails exposing (Msg)


type alias Model =
    { a2bTableOpen : Bool
    , b2aTableOpen : Bool
    , a2bTable : RelationTxsTable.Model Msg
    , b2aTable : RelationTxsTable.Model Msg
    , aggEdge : AggEdge
    }

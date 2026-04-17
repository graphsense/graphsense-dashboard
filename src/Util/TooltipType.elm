module Util.TooltipType exposing (TooltipType(..))

import Api.Data
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx as Tx


type TooltipType
    = UtxoTx Tx.UtxoTx
    | AccountTx Tx.AccountTx
    | AggEdge { leftAddress : Id, left : Maybe Api.Data.NeighborAddress, rightAddress : Id, right : Maybe Api.Data.NeighborAddress }
    | Address Id
    | TagLabel Id String
    | TagConcept Id String
    | ActorDetails String
    | Text String
    | ChangeHeuristics { confidence : Float, heuristics : List String }

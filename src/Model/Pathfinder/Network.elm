module Model.Pathfinder.Network exposing (..)

import Dict exposing (Dict)
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx as Tx exposing (Tx)


type alias Network =
    { addresses : Dict Id Address
    , txs : Dict Id Tx
    }


listTxsForAddress : Network -> Id -> List Tx
listTxsForAddress network id =
    network.txs
        |> Dict.values
        |> List.filter (Tx.hasAddress id)

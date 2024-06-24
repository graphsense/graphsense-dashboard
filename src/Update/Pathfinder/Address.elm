module Update.Pathfinder.Address exposing (removeTx)

import Model.Pathfinder.Address as Address exposing (..)
import Model.Pathfinder.Id as Id exposing (Id)
import Set


removeTx : Id -> Address -> Address
removeTx id address =
    { address
        | incomingTxs = Set.remove id address.incomingTxs
        , outgoingTxs = Set.remove id address.outgoingTxs
    }

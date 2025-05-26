module Model.Pathfinder.Error exposing (Error(..), InfoError(..), InternalError(..))

import Model.Direction exposing (Direction)
import Model.Pathfinder.Id exposing (Id)


type Error
    = InternalError InternalError
    | InfoError InfoError
    | Errors (List Error)


type InfoError
    = TxTracingThroughService Id (Maybe String)
    | NoAdjaccentTxForAddressFound Id
    | NoAdjacentTxForAddressAndNeighborFound Id Id
    | MaxChangeHopsLimitReached Int Id


type InternalError
    = AddressNotFoundInDict Id
    | TxValuesEmpty Direction Id
    | NoTxInputsOutputsFoundInDict Id

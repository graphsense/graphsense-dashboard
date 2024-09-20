module Model.Pathfinder.Error exposing (Error(..), InternalError(..))

import Model.Direction exposing (Direction)
import Model.Pathfinder.Id exposing (Id)


type Error
    = InternalError InternalError
    | Errors (List Error)


type InternalError
    = AddressNotFoundInDict Id
    | TxValuesEmpty Direction Id
    | NoTxInputsOutputsFoundInDict Id

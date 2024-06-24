module Model.Pathfinder.Details exposing (..)

import Model.Pathfinder.Details.AddressDetails as AddressDetails
import Model.Pathfinder.Details.TxDetails as TxDetails
import Model.Pathfinder.Id exposing (Id)


type Model
    = Address Id AddressDetails.Model
    | Tx Id TxDetails.Model

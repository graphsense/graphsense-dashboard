module Util.Pathfinder exposing (getAddress)

import Dict exposing (Dict)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Error as PfError
import Model.Pathfinder.Id exposing (Id)


getAddress : Dict Id Address -> Id -> Result PfError.Error Address
getAddress addresses id =
    Dict.get id addresses
        |> Maybe.map Ok
        |> Maybe.withDefault (PfError.AddressNotFoundInDict id |> PfError.InternalError |> Err)

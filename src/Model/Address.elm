module Model.Address exposing (Address, fromPathfinderId, getExposedAssets)

import Api.Data
import Dict
import Model.Pathfinder.Id as Pathfinder
import Set


type alias Address =
    { currency : String
    , address : String
    }


fromPathfinderId : Pathfinder.Id -> Address
fromPathfinderId id =
    { currency = Pathfinder.network id
    , address = Pathfinder.id id
    }


getExposedAssets : Api.Data.Address -> List String
getExposedAssets address =
    (address.currency |> String.toUpper)
        :: ((address.tokenBalances |> Maybe.map Dict.keys |> Maybe.withDefault [])
                ++ (address.totalTokensReceived |> Maybe.map Dict.keys |> Maybe.withDefault [])
                ++ (address.totalTokensSpent |> Maybe.map Dict.keys |> Maybe.withDefault [])
                |> Set.fromList
                |> Set.toList
                |> List.map String.toUpper
                |> List.sort
           )

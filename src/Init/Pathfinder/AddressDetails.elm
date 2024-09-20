module Init.Pathfinder.AddressDetails exposing (..)

import Api.Data
import Effect.Pathfinder exposing (Effect)
import Init.Pathfinder.Table.NeighborsTable as NeighborsTable
import Init.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Locale as Locale
import Model.Pathfinder.AddressDetails as AddressDetails
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)


init : Network -> Locale.Model -> Id -> Api.Data.Address -> ( AddressDetails.Model, List Effect )
init network locale addressId data =
    let
        ( txs, eff ) =
            TransactionTable.init network locale addressId data
    in
    ( { neighborsTableOpen = False
      , transactionsTableOpen = False
      , txs = txs
      , neighborsOutgoing = NeighborsTable.init data.outDegree
      , neighborsIncoming = NeighborsTable.init data.inDegree
      , addressId = addressId
      , data = data
      }
    , eff
    )

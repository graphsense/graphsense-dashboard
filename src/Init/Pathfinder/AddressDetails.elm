module Init.Pathfinder.AddressDetails exposing (init)

import Api.Data
import Basics.Extra exposing (flip)
import Config.Update as Update
import Dict exposing (Dict)
import Effect.Pathfinder exposing (Effect)
import Init.Pathfinder.Id as Id
import Init.Pathfinder.Table.NeighborsTable as NeighborsTable
import Init.Pathfinder.Table.TransactionTable as TransactionTable
import Model.DateFilter exposing (DateFilterRaw)
import Model.Pathfinder.AddressDetails as AddressDetails
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import RemoteData exposing (WebData)
import Tuple exposing (first, second)
import Update.Pathfinder.Table.RelatedAddressesTable as RelatedAddressesTable


init : Update.Config -> Network -> Dict Id (WebData Api.Data.Entity) -> DateFilterRaw -> Id -> List String -> WebData Api.Data.Address -> ( AddressDetails.Model, List Effect )
init uc network clusters dateFilterPreset addressId assets data =
    let
        txsEff =
            data
                |> RemoteData.map (\d -> TransactionTable.init uc network dateFilterPreset addressId d assets)

        related =
            data
                |> RemoteData.map (\d -> Id.initClusterId d.currency d.entity)
                |> RemoteData.andThen (flip Dict.get clusters >> Maybe.withDefault RemoteData.NotAsked)
                |> RemoteData.map
                    (\e ->
                        RelatedAddressesTable.init addressId e
                    )
    in
    ( { neighborsTableOpen = False
      , transactionsTableOpen = False
      , tokenBalancesOpen = False
      , txs = txsEff |> RemoteData.map first
      , neighborsOutgoing = RemoteData.map (.outDegree >> NeighborsTable.init) data
      , neighborsIncoming = RemoteData.map (.inDegree >> NeighborsTable.init) data
      , addressId = addressId
      , data = data
      , relatedAddresses =
            RemoteData.map first related
      , relatedAddressesTableOpen = False
      , totalReceivedDetailsOpen = False
      , balanceDetailsOpen = False
      , totalSentDetailsOpen = False
      , outgoingNeighborsTableOpen = False
      , incomingNeighborsTableOpen = False
      , copyIconChevronOpen = False
      , isClusterDetailsOpen = False
      , displayAllTagsInDetails = False
      }
    , (txsEff |> RemoteData.map second |> RemoteData.withDefault [])
        ++ (related
                |> RemoteData.map second
                |> RemoteData.withDefault []
           )
    )

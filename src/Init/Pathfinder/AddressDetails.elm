module Init.Pathfinder.AddressDetails exposing (init)

import Api.Data
import Dict exposing (Dict)
import Effect.Pathfinder exposing (Effect)
import Init.Pathfinder.Id as Id
import Init.Pathfinder.Table.NeighborsTable as NeighborsTable
import Init.Pathfinder.Table.TransactionTable as TransactionTable
import Model.DateFilter exposing (DateFilterRaw)
import Model.Locale as Locale
import Model.Pathfinder.AddressDetails as AddressDetails
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import RemoteData exposing (WebData)
import Tuple exposing (first, second)
import Update.Pathfinder.Table.RelatedAddressesTable as RelatedAddressesTable


init : Network -> Dict Id (WebData Api.Data.Entity) -> Locale.Model -> DateFilterRaw -> Id -> List String -> Api.Data.Address -> ( AddressDetails.Model, List Effect )
init network clusters locale dateFilterPreset addressId assets data =
    let
        ( txs, eff ) =
            TransactionTable.init network locale dateFilterPreset addressId data assets

        clusterId =
            Id.initClusterId data.currency data.entity

        related =
            Dict.get clusterId clusters
                |> Maybe.withDefault RemoteData.NotAsked
                |> RemoteData.map
                    (\e ->
                        RelatedAddressesTable.init addressId e
                    )
    in
    ( { neighborsTableOpen = False
      , transactionsTableOpen = False
      , tokenBalancesOpen = False
      , txs = txs
      , neighborsOutgoing = NeighborsTable.init data.outDegree
      , neighborsIncoming = NeighborsTable.init data.inDegree
      , addressId = addressId
      , data = data
      , relatedAddresses =
            RemoteData.map first related
      , relatedAddressesTableOpen = False
      , totalReceivedDetailsOpen = False
      , balanceDetailsOpen = False
      , totalSentDetailsOpen = False
      , copyIconChevronOpen = False
      , isClusterDetailsOpen = False
      , displayAllTagsInDetails = False
      }
    , eff
        ++ (related
                |> RemoteData.map second
                |> RemoteData.withDefault []
           )
    )

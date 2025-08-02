module Init.Pathfinder.AddressDetails exposing (init)

import Api.Data
import Basics.Extra exposing (flip)
import Config.Update as Update
import Dict exposing (Dict)
import Init.Pathfinder.Id as Id
import Init.Pathfinder.Table.NeighborsTable as NeighborsTable
import Init.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Address as Address
import Model.DateFilter exposing (DateFilterRaw)
import Model.Locale as Locale
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AddressDetails as AddressDetails
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import RemoteData exposing (WebData)
import Update.Pathfinder.Table.RelatedAddressesTable as RelatedAddressesTable


getExposedAssetsForAddress : Update.Config -> Address -> List String
getExposedAssetsForAddress uc address =
    let
        allAssets =
            (Id.network address.id |> String.toUpper) :: Locale.getTokenTickers uc.locale (Id.network address.id)
    in
    address.data
        |> RemoteData.map Address.getExposedAssets
        |> RemoteData.withDefault allAssets


init : Update.Config -> Network -> Dict Id (WebData Api.Data.Entity) -> Maybe DateFilterRaw -> Address -> AddressDetails.Model
init uc network clusters dateFilterPreset address =
    let
        assets =
            getExposedAssetsForAddress uc address

        txs =
            address.data
                |> RemoteData.map (\d -> TransactionTable.init uc network dateFilterPreset address.id d assets)

        related =
            address.data
                |> RemoteData.map (\d -> Id.initClusterId d.currency d.entity)
                |> RemoteData.andThen (flip Dict.get clusters >> Maybe.withDefault RemoteData.NotAsked)
                |> RemoteData.map
                    (\e ->
                        RelatedAddressesTable.init address.id e
                    )
    in
    { neighborsTableOpen = False
    , transactionsTableOpen = dateFilterPreset /= Nothing
    , tokenBalancesOpen = False
    , txs = txs
    , neighborsOutgoing = RemoteData.map (.outDegree >> NeighborsTable.init) address.data
    , neighborsIncoming = RemoteData.map (.inDegree >> NeighborsTable.init) address.data
    , address = address
    , relatedAddresses = related
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

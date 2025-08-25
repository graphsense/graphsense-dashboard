module Init.Pathfinder.AddressDetails exposing (getExposedAssetsForAddress, init)

import Config.Update as Update
import Model.Address as Address
import Model.Locale as Locale
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AddressDetails as AddressDetails
import Model.Pathfinder.Id as Id
import RemoteData


getExposedAssetsForAddress : Update.Config -> Address -> List String
getExposedAssetsForAddress uc address =
    let
        allAssets =
            (Id.network address.id |> String.toUpper) :: Locale.getTokenTickers uc.locale (Id.network address.id)
    in
    address.data
        |> RemoteData.map Address.getExposedAssets
        |> RemoteData.withDefault allAssets


init : Address -> AddressDetails.Model
init address =
    { neighborsTableOpen = False
    , transactionsTableOpen = False
    , tokenBalancesOpen = False
    , txs = RemoteData.NotAsked
    , neighborsOutgoing = RemoteData.NotAsked
    , neighborsIncoming = RemoteData.NotAsked
    , address = address
    , relatedAddresses = RemoteData.NotAsked
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

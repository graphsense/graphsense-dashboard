module Init.Pathfinder.Details.AddressDetails exposing (..)

import Init.Pathfinder.Details.TxDetails as TxDetails
import Init.Pathfinder.Table.NeighborsTable as NeighborsTable
import Init.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Pathfinder as Pathfinder exposing (Selection(..), getAddressDetailStats)
import Model.Pathfinder.Details as Details
import Model.Pathfinder.Details.AddressDetails as AddressDetails
import Model.Pathfinder.Id exposing (Id)


init : Maybe Int -> Maybe Int -> Maybe Int -> AddressDetails.Model
init nrTransactions inDegree outDegree =
    { neighborsTableOpen = False
    , transactionsTableOpen = False
    , txs = TransactionTable.init nrTransactions
    , txMinBlock = Nothing
    , txMaxBlock = Nothing
    , neighborsOutgoing = NeighborsTable.init outDegree
    , neighborsIncoming = NeighborsTable.init inDegree
    }


getAddressDetailsViewStateDefaultForAddress : Id -> Pathfinder.Model -> AddressDetails.Model
getAddressDetailsViewStateDefaultForAddress id model =
    let
        stats =
            Pathfinder.getAddressDetailStats id model Nothing
    in
    init stats.nrTxs stats.nrIncomeingNeighbors stats.nrOutgoingNeighbors


getDetailsViewStateForSelection : Pathfinder.Model -> Maybe Details.Model
getDetailsViewStateForSelection model =
    case ( model.selection, model.details ) of
        ( SelectedAddress _, Just (Details.Address id c) ) ->
            let
                stats =
                    getAddressDetailStats id model (Just c)

                txsNew =
                    c.txs

                nIn =
                    c.neighborsIncoming

                nOut =
                    c.neighborsOutgoing
            in
            Details.Address id
                { c
                    | txs = { txsNew | nrItems = stats.nrTxs }
                    , neighborsIncoming = { nIn | nrItems = stats.nrIncomeingNeighbors }
                    , neighborsOutgoing = { nOut | nrItems = stats.nrOutgoingNeighbors }
                }
                |> Just

        ( SelectedAddress id, _ ) ->
            Details.Address id (getAddressDetailsViewStateDefaultForAddress id model)
                |> Just

        ( SelectedTx _, Just (Details.Tx id c) ) ->
            Details.Tx id c
                |> Just

        ( SelectedTx id, _ ) ->
            Details.Tx id TxDetails.init
                |> Just

        ( WillSelectTx _, details ) ->
            details

        ( WillSelectAddress _, details ) ->
            details

        ( NoSelection, _ ) ->
            Nothing

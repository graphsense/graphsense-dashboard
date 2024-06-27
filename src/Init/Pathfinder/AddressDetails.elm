module Init.Pathfinder.AddressDetails exposing (..)

import Init.DateRangePicker as DateRangePicker
import Init.Pathfinder.Table.NeighborsTable as NeighborsTable
import Init.Pathfinder.Table.TransactionTable as TransactionTable
import Init.Pathfinder.TxDetails as TxDetails
import Model.DateRangePicker as DateRangePicker
import Model.Pathfinder as Pathfinder exposing (Details(..), Selection(..), getAddressDetailStats)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AddressDetails as AddressDetails
import Model.Pathfinder.Id exposing (Id)
import Msg.Pathfinder.AddressDetails exposing (Msg(..))
import Dict


init : Maybe Int -> Maybe Int -> Maybe Int -> Address -> AddressDetails.Model
init nrTransactions inDegree outDegree address =
    { neighborsTableOpen = False
    , transactionsTableOpen = False
    , txs = TransactionTable.init nrTransactions
    , txMinBlock = Nothing
    , txMaxBlock = Nothing
    , neighborsOutgoing = NeighborsTable.init outDegree
    , neighborsIncoming = NeighborsTable.init inDegree
    , dateRangePicker = DateRangePicker.init UpdateDateRangePicker
    , address = address
    }


getAddressDetailsViewStateDefaultForAddress : Id -> Pathfinder.Model -> Maybe AddressDetails.Model
getAddressDetailsViewStateDefaultForAddress id model =
    let
        stats =
            Pathfinder.getAddressDetailStats id model Nothing
    in
    Dict.get id model.network.addresses
    |> Maybe.map (init stats.nrTxs stats.nrIncomeingNeighbors stats.nrOutgoingNeighbors)


getDetailsViewStateForSelection : Pathfinder.Model -> Maybe Details
getDetailsViewStateForSelection model =
    case ( model.selection, model.details ) of
        ( SelectedAddress _, Just (AddressDetails id c) ) ->
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
            AddressDetails id
                { c
                    | txs = { txsNew | nrItems = stats.nrTxs }
                    , neighborsIncoming = { nIn | nrItems = stats.nrIncomeingNeighbors }
                    , neighborsOutgoing = { nOut | nrItems = stats.nrOutgoingNeighbors }
                }
                |> Just

        ( SelectedAddress id, _ ) ->
            (getAddressDetailsViewStateDefaultForAddress id model)
            |> Maybe.map (AddressDetails id )

        ( SelectedTx _, Just (TxDetails id c) ) ->
            TxDetails id c
                |> Just

        ( SelectedTx id, _ ) ->
            TxDetails id TxDetails.init
                |> Just

        ( WillSelectTx _, details ) ->
            details

        ( WillSelectAddress _, details ) ->
            details

        ( MultiSelect _, _ ) ->
            Nothing

        ( NoSelection, _ ) ->
            Nothing

module Model.Pathfinder exposing (..)

import Api.Data exposing (Actor)
import Config.Pathfinder exposing (Config)
import Dict exposing (Dict)
import DurationDatePicker
import Init.Pathfinder.Table.NeighborsTable as NeighborsTable
import Init.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Graph exposing (Dragging)
import Model.Graph.History as History
import Model.Graph.Transform as Transform
import Model.Pathfinder.Address as Address exposing (Address)
import Model.Pathfinder.History.Entry as Entry
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import Model.Pathfinder.Table exposing (PagedTable)
import Model.Pathfinder.Tools exposing (PointerTool)
import Model.Search as Search
import Msg.Pathfinder exposing (Msg)
import RecordSetter exposing (s_detailsViewState, s_selection)
import Time exposing (Posix)
import Model.Pathfinder.Address exposing (getNrTxs)
import RecordSetter exposing (s_outDegree)


type alias Model =
    { network : Network
    , actors : Dict String Actor
    , dragging : Dragging Id
    , selection : Selection
    , search : Search.Model
    , transform : Transform.Model Id
    , history : History.Model Entry.Model
    , view : ViewState
    , config : Config
    , dateRangePicker : DurationDatePicker.DatePicker Msg
    , fromDate : Maybe Posix
    , toDate : Maybe Posix
    , currentTime : Posix
    }


type alias ViewState =
    { detailsViewState : DetailsViewState
    , pointerTool : PointerTool
    }


type DetailsViewState
    = NoDetails
    | AddressDetails Id AddressDetailsViewState
    | TxDetails Id TxDetailsViewState


type alias TxDetailsViewState =
    { ioTableOpen : Bool
    }


type alias AddressDetailsViewState =
    { neighborsTableOpen : Bool
    , transactionsTableOpen : Bool
    , txs : PagedTable Api.Data.AddressTx
    , txMinBlock: Maybe Int
    , txMaxBlock: Maybe Int
    , neighborsIncoming : PagedTable Api.Data.NeighborAddress
    , neighborsOutgoing : PagedTable Api.Data.NeighborAddress
    }


type Selection
    = SelectedAddress Id
    | SelectedTx Id
    | WillSelectTx Id
    | WillSelectAddress Id
    | NoSelection


addressDetailsViewStateDefault : Maybe Int -> Maybe Int -> Maybe Int -> AddressDetailsViewState
addressDetailsViewStateDefault nrTransactions inDegree outDegree =
    { neighborsTableOpen = False
    , transactionsTableOpen = False
    , txs = TransactionTable.init nrTransactions
    , txMinBlock = Nothing
    , txMaxBlock = Nothing
    , neighborsOutgoing = NeighborsTable.init outDegree
    , neighborsIncoming = NeighborsTable.init inDegree
    }


getTxDetailsDefaultState : TxDetailsViewState
getTxDetailsDefaultState =
    { ioTableOpen = False }


getLoadedAddress : Model -> Id -> Maybe Address
getLoadedAddress m id =
    Dict.get id m.network.addresses


getAddressDetailStats: Id -> Model -> Maybe AddressDetailsViewState -> {nrTxs: Maybe Int, nrIncomeingNeighbors : Maybe Int, nrOutgoingNeighbors: Maybe Int }
getAddressDetailStats id model madvs = 
    let
            maddress =
                Dict.get id model.network.addresses

            nrTxs = case madvs of 
                Just advs -> case (advs.txMinBlock, advs.txMaxBlock) of
                            (Just _, Just _) -> Nothing
                            _ -> maddress |> Maybe.andThen Address.getNrTxs
                _ -> Nothing

            indegree =
                maddress |> Maybe.andThen Address.getInDegree

            outdegree =
                maddress |> Maybe.andThen Address.getOutDegree
        in
            {nrTxs = nrTxs, nrIncomeingNeighbors = indegree, nrOutgoingNeighbors = outdegree}
getAddressDetailsViewStateDefaultForAddress : Id -> Model -> AddressDetailsViewState
getAddressDetailsViewStateDefaultForAddress id model =
    let
        stats = getAddressDetailStats id model Nothing
    in
    addressDetailsViewStateDefault stats.nrTxs stats.nrIncomeingNeighbors stats.nrOutgoingNeighbors


getDetailsViewStateForSelection : Model -> DetailsViewState
getDetailsViewStateForSelection model =
    case ( model.selection, model.view.detailsViewState ) of
        ( SelectedAddress _, AddressDetails id c ) ->
            let
                stats = getAddressDetailStats id model (Just c)

                txsNew =
                    c.txs
                nIn = c.neighborsIncoming
                nOut = c.neighborsOutgoing
            in
            AddressDetails id { c | 
                                txs = { txsNew | nrItems = stats.nrTxs}
                                , neighborsIncoming = {nIn | nrItems = stats.nrIncomeingNeighbors}
                                , neighborsOutgoing = {nOut | nrItems = stats.nrOutgoingNeighbors} } 

        ( SelectedAddress id, _ ) ->
            AddressDetails id (getAddressDetailsViewStateDefaultForAddress id model)

        ( SelectedTx _, TxDetails id c ) ->
            TxDetails id c

        ( SelectedTx id, _ ) ->
            TxDetails id getTxDetailsDefaultState

        ( WillSelectTx _, details ) ->
            details

        ( WillSelectAddress _, details ) ->
            details

        ( NoSelection, _ ) ->
            NoDetails


isDetailsViewVisible : Model -> Bool
isDetailsViewVisible model =
    not (model.selection == NoSelection)


setViewState : (ViewState -> ViewState) -> Model -> Model
setViewState fn model =
    { model | view = fn model.view }


closeDetailsView : Model -> Model
closeDetailsView =
    (setViewState <| s_detailsViewState NoDetails) >> s_selection NoSelection

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
    , neighborsOutgoing = NeighborsTable.init outDegree
    , neighborsIncoming = NeighborsTable.init inDegree
    }


getTxDetailsDefaultState : TxDetailsViewState
getTxDetailsDefaultState =
    { ioTableOpen = False }


getLoadedAddress : Model -> Id -> Maybe Address
getLoadedAddress m id =
    Dict.get id m.network.addresses


getAddressDetailsViewStateDefaultForAddress : Id -> Model -> AddressDetailsViewState
getAddressDetailsViewStateDefaultForAddress id model =
    let
        maddress =
            Dict.get id model.network.addresses

        nrTxs =
            maddress |> Maybe.andThen Address.getNrTxs

        indegree =
            maddress |> Maybe.andThen Address.getInDegree

        outdegree =
            maddress |> Maybe.andThen Address.getOutDegree
    in
    addressDetailsViewStateDefault nrTxs indegree outdegree


getDetailsViewStateForSelection : Model -> DetailsViewState
getDetailsViewStateForSelection model =
    case ( model.selection, model.view.detailsViewState ) of
        ( SelectedAddress _, AddressDetails id c ) ->
            let
                maddress =
                    Dict.get id model.network.addresses

                nrTxs =
                    maddress |> Maybe.andThen Address.getNrTxs

                txsNew =
                    c.txs
            in
            AddressDetails id { c | txs = { txsNew | nrItems = nrTxs } }

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

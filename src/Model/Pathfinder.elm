module Model.Pathfinder exposing (..)

import Api.Data
import Dict exposing (Dict)
import Model.Graph exposing (Dragging)
import Model.Graph.History as History
import Model.Graph.Transform as Transform
import Model.Pathfinder.History.Entry as Entry
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import Model.Search as Search


type alias Model =
    { network : Network
    , dragging : Dragging Id
    , selection : List Id
    , search : Search.Model
    , transform : Transform.Model Id
    , history : History.Model Entry.Model
    , view : ViewState
    }


type alias ViewState =
    { detailsViewState : DetailsViewState
    }


type alias TableState =
    { open : Bool
    }


type alias AddressDetailsViewState =
    { addressTableOpen : Bool
    , transactionsTableOpen : Bool
    }


type DetailsViewState
    = NoDetails
    | Address Id AddressDetailsViewState (Maybe Api.Data.Address)


isDetailsViewVisible : Model -> Bool
isDetailsViewVisible model =
    not (model.view.detailsViewState == NoDetails)


setViewState : (ViewState -> ViewState) -> Model -> Model
setViewState fn model =
    { model | view = fn model.view }


setDetailsViewState : DetailsViewState -> ViewState -> ViewState
setDetailsViewState val model =
    { model | detailsViewState = val }


closeDetailsView : Model -> Model
closeDetailsView =
    setViewState <| setDetailsViewState NoDetails


toggleAddressDetailsTable : Model -> Model
toggleAddressDetailsTable m =
    case m.view.detailsViewState of
        Address id ad data ->
            (setViewState <| setDetailsViewState (Address id { ad | addressTableOpen = not ad.addressTableOpen } data)) m

        _ ->
            m


toggleTransactionDetailsTable : Model -> Model
toggleTransactionDetailsTable m =
    case m.view.detailsViewState of
        Address id ad data ->
            (setViewState <| setDetailsViewState (Address id { ad | transactionsTableOpen = not ad.transactionsTableOpen } data)) m

        _ ->
            m

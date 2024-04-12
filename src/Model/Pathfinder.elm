module Model.Pathfinder exposing (..)

import Dict
import Model.Graph exposing (Dragging)
import Model.Graph.History as History
import Model.Graph.Transform as Transform
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.History.Entry as Entry
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import Model.Search as Search


type alias Model =
    { network : Network
    , dragging : Dragging Id
    , selection : Selection
    , search : Search.Model
    , transform : Transform.Model Id
    , history : History.Model Entry.Model
    , view : ViewState
    }


type alias ViewState =
    { detailsViewState : DetailsViewState
    }


type alias AddressDetailsViewState =
    { addressTableOpen : Bool
    , transactionsTableOpen : Bool
    }


type Selection
    = SelectedAddress Id
    | NoSelection


type DetailsViewState
    = NoDetails
    | AddressDetails Id AddressDetailsViewState


getLoadedAddress : Model -> Id -> Maybe Address
getLoadedAddress m id =
    Dict.get id m.network.addresses


getDetailsViewStateForSelection : Model -> DetailsViewState
getDetailsViewStateForSelection model =
    case ( model.selection, model.view.detailsViewState ) of
        ( SelectedAddress _, AddressDetails id c ) ->
            AddressDetails id c

        ( SelectedAddress id, _ ) ->
            AddressDetails id { addressTableOpen = False, transactionsTableOpen = False }

        ( NoSelection, _ ) ->
            NoDetails


isDetailsViewVisible : Model -> Bool
isDetailsViewVisible model =
    not (model.selection == NoSelection)


setViewState : (ViewState -> ViewState) -> Model -> Model
setViewState fn model =
    { model | view = fn model.view }


setSelection : Selection -> Model -> Model
setSelection val model =
    { model | selection = val }


setDetailsViewState : DetailsViewState -> ViewState -> ViewState
setDetailsViewState val model =
    { model | detailsViewState = val }


closeDetailsView : Model -> Model
closeDetailsView =
    (setViewState <| setDetailsViewState NoDetails) >> setSelection NoSelection


toggleAddressDetailsTable : Model -> Model
toggleAddressDetailsTable m =
    case m.view.detailsViewState of
        AddressDetails id ad ->
            (setViewState <| setDetailsViewState (AddressDetails id { ad | addressTableOpen = not ad.addressTableOpen })) m

        _ ->
            m


toggleTransactionDetailsTable : Model -> Model
toggleTransactionDetailsTable m =
    case m.view.detailsViewState of
        AddressDetails id ad ->
            (setViewState <| setDetailsViewState (AddressDetails id { ad | transactionsTableOpen = not ad.transactionsTableOpen })) m

        _ ->
            m

module Model.Pathfinder exposing (..)

import Api.Data exposing (Actor)
import Dict exposing (Dict)
import Model.Graph exposing (Dragging)
import Model.Graph.History as History
import Model.Graph.Transform as Transform
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.History.Entry as Entry
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import Model.Search as Search
import RecordSetter exposing (s_detailsViewState, s_selection)


type alias Model =
    { network : Network
    , actors : Dict String Actor
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


closeDetailsView : Model -> Model
closeDetailsView =
    (setViewState <| s_detailsViewState NoDetails) >> s_selection NoSelection


toggleAddressDetailsTable : Model -> Model
toggleAddressDetailsTable m =
    case m.view.detailsViewState of
        AddressDetails id ad ->
            (setViewState <| s_detailsViewState (AddressDetails id { ad | addressTableOpen = not ad.addressTableOpen })) m

        _ ->
            m


toggleTransactionDetailsTable : Model -> Model
toggleTransactionDetailsTable m =
    case m.view.detailsViewState of
        AddressDetails id ad ->
            (setViewState <| s_detailsViewState (AddressDetails id { ad | transactionsTableOpen = not ad.transactionsTableOpen })) m

        _ ->
            m

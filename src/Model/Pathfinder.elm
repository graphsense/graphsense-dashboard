module Model.Pathfinder exposing (..)

import Api.Data exposing (Actor)
import Config.Pathfinder exposing (Config)
import Dict exposing (Dict)
import DurationDatePicker
import Model.Graph exposing (Dragging)
import Model.Graph.History as History
import Model.Graph.Transform as Transform
import Model.Pathfinder.Address as Address exposing (Address)
import Model.Pathfinder.Details as Details
import Model.Pathfinder.Details.AddressDetails as AddressDetails
import Model.Pathfinder.History.Entry as Entry
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import Model.Pathfinder.Tools exposing (PointerTool)
import Model.Search as Search
import Msg.Pathfinder exposing (Msg)
import Theme.Svg.GraphComponents as GraphComponents
import Time exposing (Posix)


unit : Float
unit =
    GraphComponents.addressNodeNodeFrameDimensions.width


type alias Model =
    { network : Network
    , actors : Dict String Actor
    , tags : Dict Id Api.Data.AddressTags
    , dragging : Dragging Id
    , selection : Selection
    , search : Search.Model
    , transform : Transform.Model Id
    , history : History.Model Entry.Model
    , details : Maybe Details.Model
    , config : Config
    , dateRangePicker : DurationDatePicker.DatePicker Msg
    , fromDate : Maybe Posix
    , toDate : Maybe Posix
    , currentTime : Posix
    , pointerTool : PointerTool
    , ctrlPressed : Bool
    , isDirty : Bool
    }


type Selection
    = SelectedAddress Id
    | SelectedTx Id
    | WillSelectTx Id
    | WillSelectAddress Id
    | NoSelection


getLoadedAddress : Model -> Id -> Maybe Address
getLoadedAddress m id =
    Dict.get id m.network.addresses


getAddressDetailStats : Id -> Model -> Maybe AddressDetails.Model -> { nrTxs : Maybe Int, nrIncomeingNeighbors : Maybe Int, nrOutgoingNeighbors : Maybe Int }
getAddressDetailStats id model madvs =
    let
        maddress =
            Dict.get id model.network.addresses

        nrTxs =
            case madvs of
                Just advs ->
                    case ( advs.txMinBlock, advs.txMaxBlock ) of
                        ( Just _, Just _ ) ->
                            Nothing

                        _ ->
                            maddress |> Maybe.andThen Address.getNrTxs

                _ ->
                    Nothing

        indegree =
            maddress |> Maybe.andThen Address.getInDegree

        outdegree =
            maddress |> Maybe.andThen Address.getOutDegree
    in
    { nrTxs = nrTxs, nrIncomeingNeighbors = indegree, nrOutgoingNeighbors = outdegree }


isDetailsViewVisible : Model -> Bool
isDetailsViewVisible model =
    not (model.selection == NoSelection)

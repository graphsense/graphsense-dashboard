module Model.Pathfinder exposing (Details(..), HavingTags(..), Hovered(..), Model, MultiSelectOptions(..), Selection(..), getAddressDetailStats, getLoadedAddress, unit)

import Api.Data exposing (Actor, Entity)
import Config.Pathfinder exposing (Config)
import Dict exposing (Dict)
import Model.Graph exposing (Dragging)
import Model.Graph.History as History
import Model.Graph.Transform as Transform
import Model.Pathfinder.Address as Address exposing (Address)
import Model.Pathfinder.AddressDetails as AddressDetails
import Model.Pathfinder.Colors exposing (ScopedColorAssignment)
import Model.Pathfinder.ContextMenu exposing (ContextMenu)
import Model.Pathfinder.History.Entry as Entry
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import Model.Pathfinder.Tools exposing (PointerTool, ToolbarHovercardModel)
import Model.Pathfinder.Tooltip exposing (Tooltip)
import Model.Pathfinder.TxDetails as TxDetails
import Model.Search as Search
import RemoteData exposing (WebData)
import Theme.Svg.GraphComponents as GraphComponents
import Time exposing (Posix)
import Util.Annotations exposing (AnnotationModel)


unit : Float
unit =
    GraphComponents.addressNodeNodeFrame_details.width


type alias Model =
    { network : Network
    , actors : Dict String Actor
    , tagSummaries : Dict Id HavingTags
    , clusters : Dict Id Entity
    , colors : ScopedColorAssignment
    , annotations : AnnotationModel
    , dragging : Dragging Id
    , selection : Selection
    , hovered : Hovered
    , search : Search.Model
    , transform : Transform.Model Id
    , history : History.Model Entry.Model
    , details : Maybe Details
    , config : Config
    , currentTime : Posix
    , pointerTool : PointerTool
    , modPressed : Bool
    , isDirty : Bool
    , tooltip : Maybe Tooltip
    , toolbarHovercard : Maybe ToolbarHovercardModel
    , contextMenu : Maybe ContextMenu
    , name : String
    }


type HavingTags
    = LoadingTags
    | HasTagSummary Api.Data.TagSummary
    | HasExchangeTagOnly
    | HasTags Bool -- whether includes an exchange tag
    | NoTags


type Selection
    = SelectedAddress Id
    | SelectedTx Id
    | MultiSelect (List MultiSelectOptions)
    | WillSelectTx Id
    | WillSelectAddress Id
    | NoSelection


type Hovered
    = HoveredTx Id
    | HoveredAddress Id
    | NoHover


type MultiSelectOptions
    = MSelectedAddress Id
    | MSelectedTx Id


type Details
    = AddressDetails Id (WebData AddressDetails.Model)
    | TxDetails Id TxDetails.Model


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
                    case ( advs.txs.txMinBlock, advs.txs.txMaxBlock ) of
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

module Model.Pathfinder exposing (Details(..), HavingTags(..), Hovered(..), Model, MultiSelectOptions(..), Selection(..), getHavingTags, getLoadedAddress, getSortedConceptsByWeight, getSortedLabelSummariesByRelevance, unit)

import Api.Data exposing (Actor, Entity)
import Config.Pathfinder exposing (Config)
import Dict exposing (Dict)
import Model.Graph exposing (Dragging)
import Model.Graph.History as History
import Model.Graph.Transform as Transform
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AddressDetails as AddressDetails
import Model.Pathfinder.CheckingNeighbors as CheckingNeighbors
import Model.Pathfinder.Colors exposing (ScopedColorAssignment)
import Model.Pathfinder.ContextMenu exposing (ContextMenu)
import Model.Pathfinder.History.Entry as Entry
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import Model.Pathfinder.RelationDetails as RelationDetails
import Model.Pathfinder.Tools exposing (PointerTool, ToolbarHovercardModel)
import Model.Pathfinder.TxDetails as TxDetails
import Model.Search as Search
import RemoteData exposing (WebData)
import Route.Pathfinder exposing (Route)
import Theme.Svg.GraphComponents as GraphComponents
import Tuple
import Util.Annotations exposing (AnnotationModel)


unit : Float
unit =
    GraphComponents.addressNodeNodeFrame_details.width


type alias Model =
    { route : Route
    , network : Network
    , actors : Dict String Actor
    , tagSummaries : Dict Id HavingTags
    , clusters : Dict Id (WebData Entity)
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
    , pointerTool : PointerTool
    , modPressed : Bool
    , isDirty : Bool
    , helpDropdownOpen : Bool
    , toolbarHovercard : Maybe ToolbarHovercardModel
    , contextMenu : Maybe ContextMenu
    , name : String
    , checkingNeighbors : CheckingNeighbors.Model
    }


type HavingTags
    = LoadingTags
    | HasTagSummaryWithCluster Api.Data.TagSummary
    | HasTagSummaryWithoutCluster Api.Data.TagSummary
    | HasTagSummaryOnlyWithCluster Api.Data.TagSummary
    | HasTagSummaries { withCluster : Api.Data.TagSummary, withoutCluster : Api.Data.TagSummary }
    | HasExchangeTagOnly
    | HasTags Bool -- whether includes an exchange tag
    | NoTagsWithoutCluster
    | NoTags


type Selection
    = SelectedAddress Id
    | SelectedTx Id
    | SelectedAggEdge ( Id, Id )
    | MultiSelect (List MultiSelectOptions)
    | WillSelectTx Id
    | WillSelectAddress Id
    | WillSelectAggEdge ( Id, Id )
    | NoSelection


type Hovered
    = HoveredTx Id
    | HoveredAggEdge ( Id, Id )
    | HoveredConversionEdge ( Id, Id )
    | HoveredAddress Id
    | NoHover


type MultiSelectOptions
    = MSelectedAddress Id
    | MSelectedTx Id


type Details
    = AddressDetails Id AddressDetails.Model
    | TxDetails Id TxDetails.Model
    | RelationDetails ( Id, Id ) RelationDetails.Model


getLoadedAddress : Model -> Id -> Maybe Address
getLoadedAddress m id =
    Dict.get id m.network.addresses


getHavingTags : Model -> Id -> HavingTags
getHavingTags model id_ =
    Dict.get id_ model.tagSummaries
        |> Maybe.withDefault NoTags


getSortedLabelSummariesByRelevance : Api.Data.TagSummary -> List ( String, Api.Data.LabelSummary )
getSortedLabelSummariesByRelevance =
    .labelSummary >> Dict.toList >> List.sortBy (Tuple.second >> .relevance) >> List.reverse


getSortedConceptsByWeight : Api.Data.TagSummary -> List String
getSortedConceptsByWeight =
    .conceptTagCloud
        >> Dict.toList
        >> List.sortBy (Tuple.second >> .weighted)
        >> List.map Tuple.first
        >> List.reverse

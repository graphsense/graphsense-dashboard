module PluginInterface.Msg exposing (InMsg(..), InMsgPathfinder(..), OutMsg(..), OutMsgPathfinder(..), mapOutMsg)

import Api.Data
import Effect.Api as Api
import Json.Encode
import Model.Address exposing (Address)
import Model.Dialog
import Model.Entity exposing (Entity)
import Model.Notification exposing (Notification)
import Model.Pathfinder.Address as Pathfinder
import Route.Pathfinder exposing (PathHopType)
import Update.Dialog



{- Plugins can communicate with core via these messages -}


type OutMsg msg addressMsg entityMsg
    = -- send addressMsg to all address nodes in the graph which match the one in `Address`
      UpdateAddresses Address addressMsg
    | UpdateAddressesByRootAddress Address addressMsg
    | UpdateAddressesByEntityPathfinder Api.Data.Cluster addressMsg
      -- push url to the browser history (updates the URL in the browser address bar)
    | PushUrl String
      -- trigger a browser history step back
    | Back Int
      -- retrieve entities for the given list of addresses
    | GetEntitiesForAddresses (List Address) (List ( Address, Api.Data.Cluster ) -> msg)
      -- retrieve entities for the given list of entities
    | GetEntities (List Entity) (List Api.Data.Cluster -> msg)
      -- load given value as deserialization of a pathfinder graph
    | Deserialize String Json.Encode.Value
      -- send value to javascript (further processed in the plugin's root js)
    | SendToPort Json.Encode.Value
      -- send a request to the Graphsense API
    | ApiRequest (Api.Effect msg)
      -- show dialog
    | ShowDialog (Model.Dialog.Model msg)
      -- close dialog
    | CloseDialog
      -- show notification
    | ShowNotification Notification
      -- pathfinder Specific msgs
    | OutMsgsPathfinder (OutMsgPathfinder msg)


type OutMsgPathfinder msg
    = -- retrieve a serialized state of the pathfinder grapn
      GetPathfinderGraphJson (Json.Encode.Value -> msg)
    | ShowPathsInPathfinder String (List (List PathHopType)) --legacy for backwards compatibility
    | ShowPathsInPathfinderWithConfig String { outgoing : Bool } (List (List PathHopType))
    | GetAddressesShown (List Pathfinder.Address -> msg)



{- Core can communicate with plugins via these messages -}


type InMsg
    = -- User clicked to e.g. the graph or anything outside things with a handler attached (roughly corresponds to UserClickedLayout)
      ClickedOnNeutralGround
    | CoreGotStatsUpdate Api.Data.Stats
      -- when addresses are added to the model
    | AddressesAdded (List Address)
      -- when entities are added to the model
    | EntitiesAdded (List Entity)
    | InMsgsPathfinder InMsgPathfinder
    | ClosedTooltip (Maybe { context : String, domId : String })
    | Reset


type InMsgPathfinder
    = -- retrieve a serialized state of the pathfinder graph
      PathfinderGraphChanged


mapOutMsg : String -> (msgA -> msgB) -> (addressMsgA -> addressMsgB) -> (entityMsgA -> entityMsgB) -> OutMsg msgA addressMsgA entityMsgA -> OutMsg msgB addressMsgB entityMsgB
mapOutMsg namespace mapMsg mapAddressMsg _ outMsg =
    case outMsg of
        UpdateAddresses a addressMsg ->
            mapAddressMsg addressMsg
                |> UpdateAddresses a

        UpdateAddressesByRootAddress a addressMsg ->
            mapAddressMsg addressMsg
                |> UpdateAddressesByRootAddress a

        UpdateAddressesByEntityPathfinder a addressMsg ->
            mapAddressMsg addressMsg
                |> UpdateAddressesByEntityPathfinder a

        PushUrl u ->
            PushUrl u

        Back steps ->
            Back steps

        GetEntitiesForAddresses a b ->
            (b >> mapMsg)
                |> GetEntitiesForAddresses a

        GetEntities a b ->
            (b >> mapMsg)
                |> GetEntities a

        OutMsgsPathfinder (GetPathfinderGraphJson msg) ->
            ((msg >> mapMsg) |> GetPathfinderGraphJson) |> OutMsgsPathfinder

        OutMsgsPathfinder (GetAddressesShown msg) ->
            ((msg >> mapMsg) |> GetAddressesShown) |> OutMsgsPathfinder

        OutMsgsPathfinder (ShowPathsInPathfinder s p) ->
            ShowPathsInPathfinder s p |> OutMsgsPathfinder

        OutMsgsPathfinder (ShowPathsInPathfinderWithConfig s c p) ->
            ShowPathsInPathfinderWithConfig s c p |> OutMsgsPathfinder

        Deserialize filename json ->
            Deserialize filename json

        SendToPort value ->
            [ Json.Encode.string namespace
            , value
            ]
                |> Json.Encode.list identity
                |> SendToPort

        ApiRequest effect ->
            Api.map mapMsg effect
                |> ApiRequest

        ShowDialog dialog ->
            Update.Dialog.mapMsg mapMsg dialog
                |> ShowDialog

        CloseDialog ->
            CloseDialog

        ShowNotification a ->
            ShowNotification a

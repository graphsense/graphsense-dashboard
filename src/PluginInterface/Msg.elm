module PluginInterface.Msg exposing (InMsg(..), InMsgPathfinder(..), OutMsg(..), OutMsgPathfinder(..), mapOutMsg)

import Api.Data
import Browser.Dom
import Effect.Api as Api
import Json.Encode
import Model.Address exposing (Address)
import Model.Dialog
import Model.Entity exposing (Entity)
import Model.Graph.Id as Id
import Model.Notification exposing (Notification)
import Model.Pathfinder.Address as Pathfinder
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tooltip exposing (TooltipMessages, mapMsgTooltipMsg)
import Route.Pathfinder exposing (PathHopType)
import Update.Dialog



{- Plugins can communicate with core via these messages -}


type OutMsg msg addressMsg entityMsg
    = -- popup the graph's browser
      ShowBrowser
      -- send addressMsg to all address nodes in the graph which match the one in `Address`
    | UpdateAddresses Address addressMsg
    | UpdateAddressesByRootAddress Address addressMsg
    | UpdateAddressesByEntityPathfinder Api.Data.Entity addressMsg
      -- send entityMsg to the entity of all address nodes in the graph which match the one in `Address`
      -- core calls the `update.updateAddress` hook
    | UpdateAddressEntities Address entityMsg
      -- send entityMsg to all entity nodes in the graph which match the one in `Entity`
      -- core calls the `update.updateEntity` hook
    | UpdateEntities Entity entityMsg
      -- send entityMsg to all entity nodes in the graph whose root address matches the one in `Address`
      -- core calls the `update.updateEntity` hook
    | UpdateEntitiesByRootAddress Address entityMsg
      -- push url to the browser history (updates the URL in the browser address bar)
    | PushUrl String
      -- retrieve entities for the given list of addresses
    | GetEntitiesForAddresses (List Address) (List ( Address, Api.Data.Entity ) -> msg)
      -- retrieve entities for the given list of entities
    | GetEntities (List Entity) (List Api.Data.Entity -> msg)
      -- retrieve a serialized state of the graph
    | GetSerialized (Json.Encode.Value -> msg)
      -- load given value as deserialization of graph
    | Deserialize String Json.Encode.Value
      -- get address dom element for the given address node id
    | GetAddressDomElement Id.AddressId (Result Browser.Dom.Error Browser.Dom.Element -> msg)
      -- send value to javascript (further processed in the plugin's root js)
    | SendToPort Json.Encode.Value
      -- send a request to the Graphsense API
    | ApiRequest (Api.Effect msg)
      -- show dialog
    | ShowDialog (Model.Dialog.Model msg)
      -- close dialog
    | CloseDialog
      -- open a tooltip
    | OpenTooltip { context : String, domId : String } (TooltipMessages msg)
      -- close a tooltip
    | CloseTooltip { context : String, domId : String } Bool
      -- load address into graph
    | LoadAddressIntoGraph Address
      -- show notification
    | ShowNotification Notification
      -- pathfinder Specific msgs
    | OutMsgsPathfinder (OutMsgPathfinder msg)


type OutMsgPathfinder msg
    = -- retrieve a serialized state of the pathfinder grapn
      GetPathfinderGraphJson (Json.Encode.Value -> msg)
    | ShowPathsInPathfinder String (List (List PathHopType))
    | GetAddressesShown (List Pathfinder.Address -> msg)



{- Core can communicate with plugins via these messages -}


type InMsg
    = -- User clicked to e.g. the graph or anything outside things with a handler attached (roughly corresponds to UserClickedLayout)
      ClickedOnNeutralGround
    | CoreGotStatsUpdate Api.Data.Stats
    | AddressesAdded (List Address)
    | AddressesAddedPathfinder (List ( Address, Api.Data.Entity ))
    | InMsgsPathfinder InMsgPathfinder
    | ClosedTooltip (Maybe { context : String, domId : String })
    | Reset


type InMsgPathfinder
    = -- retrieve a serialized state of the pathfinder graph
      PathfinderGraphChanged


mapOutMsg : String -> (msgA -> msgB) -> (addressMsgA -> addressMsgB) -> (entityMsgA -> entityMsgB) -> OutMsg msgA addressMsgA entityMsgA -> OutMsg msgB addressMsgB entityMsgB
mapOutMsg namespace mapMsg mapAddressMsg mapEntityMsg outMsg =
    case outMsg of
        ShowBrowser ->
            ShowBrowser

        UpdateAddresses a addressMsg ->
            mapAddressMsg addressMsg
                |> UpdateAddresses a

        UpdateAddressesByRootAddress a addressMsg ->
            mapAddressMsg addressMsg
                |> UpdateAddressesByRootAddress a

        UpdateAddressesByEntityPathfinder a addressMsg ->
            mapAddressMsg addressMsg
                |> UpdateAddressesByEntityPathfinder a

        UpdateEntities e entityMsg ->
            mapEntityMsg entityMsg
                |> UpdateEntities e

        UpdateEntitiesByRootAddress a entityMsg ->
            mapEntityMsg entityMsg
                |> UpdateEntitiesByRootAddress a

        UpdateAddressEntities a entityMsg ->
            mapEntityMsg entityMsg
                |> UpdateAddressEntities a

        PushUrl u ->
            PushUrl u

        GetEntitiesForAddresses a b ->
            (b >> mapMsg)
                |> GetEntitiesForAddresses a

        GetEntities a b ->
            (b >> mapMsg)
                |> GetEntities a

        GetSerialized msg ->
            (msg >> mapMsg) |> GetSerialized

        OutMsgsPathfinder (GetPathfinderGraphJson msg) ->
            ((msg >> mapMsg) |> GetPathfinderGraphJson) |> OutMsgsPathfinder

        OutMsgsPathfinder (GetAddressesShown msg) ->
            ((msg >> mapMsg) |> GetAddressesShown) |> OutMsgsPathfinder

        OutMsgsPathfinder (ShowPathsInPathfinder s p) ->
            ShowPathsInPathfinder s p |> OutMsgsPathfinder

        Deserialize filename json ->
            Deserialize filename json

        GetAddressDomElement element msg ->
            (msg >> mapMsg) |> GetAddressDomElement element

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

        OpenTooltip x msgs ->
            OpenTooltip x (mapMsgTooltipMsg msgs mapMsg)

        CloseTooltip x delayed ->
            CloseTooltip x delayed

        LoadAddressIntoGraph a ->
            LoadAddressIntoGraph a

        ShowNotification a ->
            ShowNotification a

module PluginInterface.Msg exposing (..)

import Api.Data
import Browser.Dom
import Effect.Api as Api exposing (..)
import Json.Encode
import Model.Address exposing (Address)
import Model.Dialog
import Model.Entity exposing (Entity)
import Model.Graph.Id as Id
import Update.Dialog



{- Plugins can communicate with core via these messages -}


type OutMsg msg addressMsg entityMsg
    = -- popup the graph's browser
      ShowBrowser
      -- send addressMsg to all address nodes in the graph which match the one in `Address`
    | UpdateAddresses Address addressMsg
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


mapOutMsg : String -> (msgA -> msgB) -> (addressMsgA -> addressMsgB) -> (entityMsgA -> entityMsgB) -> OutMsg msgA addressMsgA entityMsgA -> OutMsg msgB addressMsgB entityMsgB
mapOutMsg namespace mapMsg mapAddressMsg mapEntityMsg outMsg =
    case outMsg of
        ShowBrowser ->
            ShowBrowser

        UpdateAddresses a addressMsg ->
            mapAddressMsg addressMsg
                |> UpdateAddresses a

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

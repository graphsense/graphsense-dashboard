module PluginInterface.Msg exposing (..)

import Api.Data
import Browser.Dom
import Json.Encode
import Model.Address exposing (Address)
import Model.Entity exposing (Entity)
import Model.Graph.Id as Id


type OutMsg msg addressMsg entityMsg
    = ShowBrowser
    | UpdateAddresses Address addressMsg
    | UpdateAddressEntities Address entityMsg
    | UpdateEntities Entity entityMsg
    | PushUrl String
    | GetEntitiesForAddresses (List Address) (List ( Address, Api.Data.Entity ) -> msg)
    | GetEntities (List Entity) (List Api.Data.Entity -> msg)
    | GetSerialized (Json.Encode.Value -> msg)
    | Deserialize String Json.Encode.Value
    | GetAddressDomElement Id.AddressId (Result Browser.Dom.Error Browser.Dom.Element -> msg)
    | SendToPort Json.Encode.Value


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

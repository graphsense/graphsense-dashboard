module PluginInterface.Msg exposing (..)

import Api.Data
import Json.Encode
import Model.Address exposing (Address)
import Model.Entity exposing (Entity)


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


mapOutMsg : (msgA -> msgB) -> (addressMsgA -> addressMsgB) -> (entityMsgA -> entityMsgB) -> OutMsg msgA addressMsgA entityMsgA -> OutMsg msgB addressMsgB entityMsgB
mapOutMsg mapMsg mapAddressMsg mapEntityMsg outMsg =
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

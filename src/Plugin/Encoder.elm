module Plugin.Encoder exposing (..)

import Json.Encode exposing (Value)
import Plugin.Model exposing (OutMsg(..))


outMsg : (msg -> Value) -> (addressMsg -> Value) -> (entityMsg -> Value) -> OutMsg msg addressMsg entityMsg -> OutMsg Value Value Value
outMsg msgEncoder addressMsgEncoder entityMsgEncoder ms =
    case ms of
        ShowBrowser ->
            ShowBrowser

        UpdateAddresses a m ->
            addressMsgEncoder m
                |> UpdateAddresses a

        UpdateAddressEntities a m ->
            entityMsgEncoder m
                |> UpdateAddressEntities a

        UpdateEntities a m ->
            entityMsgEncoder m
                |> UpdateEntities a

        PushGraphUrl url ->
            PushGraphUrl url

        GetEntitiesForAddresses addresses m ->
            m
                >> msgEncoder
                |> GetEntitiesForAddresses addresses

        GetEntities entities m ->
            m
                >> msgEncoder
                |> GetEntities entities

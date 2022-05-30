module Plugin.Encoder exposing (..)

import Json.Encode exposing (Value)
import Plugin.Model exposing (OutMsg(..))


outMsg : (msg -> Value) -> OutMsg msg -> OutMsg Value
outMsg encoder ms =
    case ms of
        ShowBrowser ->
            ShowBrowser

        UpdateAddresses a m ->
            encoder m
                |> UpdateAddresses a

        PushUrl url ->
            PushUrl url

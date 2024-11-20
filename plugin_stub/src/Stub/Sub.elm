module Stub.Sub exposing (subscriptions, inPort)

import Json.Decode
import Stub.Model exposing (Model)
import Stub.Msg exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


inPort : Json.Decode.Value -> Msg
inPort value =
    Debug.todo "A value from the plugin's port"

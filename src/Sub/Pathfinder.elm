module Sub.Pathfinder exposing (subscriptions)

import Browser.Events
import Json.Decode as Decode
import Model.Graph exposing (Dragging(..))
import Model.Pathfinder exposing (Model)
import Msg.Pathfinder exposing (Msg(..))
import Sub.Graph.Transform as Transform


keyDecoder : (String -> Msg) -> Decode.Decoder Msg
keyDecoder kMap =
    Decode.map kMap (Decode.field "key" Decode.string)


toKeyDown : String -> Msg
toKeyDown keyValue =
    case String.uncons keyValue of
        Just ( _, "" ) ->
            NoOp

        -- Normal keypress
        Just ( 'C', "ontrol" ) ->
            -- https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values
            UserPressedCtrlKey

        _ ->
            NoOp


toKeyUp : String -> Msg
toKeyUp keyValue =
    case String.uncons keyValue of
        Just ( _, "" ) ->
            NoOp

        -- Normal keypress
        Just ( 'C', "ontrol" ) ->
            -- https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values
            UserReleasedCtrlKey

        _ ->
            NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    [ case model.dragging of
        NoDragging ->
            Sub.none

        _ ->
            Browser.Events.onMouseUp (Decode.succeed UserReleasesMouseButton)
    , Transform.subscriptions AnimationFrameDeltaForTransform model.transform
    , Browser.Events.onKeyDown (keyDecoder toKeyDown)
    , Browser.Events.onKeyUp (keyDecoder toKeyUp)
    , Browser.Events.onVisibilityChange (\_ -> UserReleasedCtrlKey)
    ]
        |> Sub.batch

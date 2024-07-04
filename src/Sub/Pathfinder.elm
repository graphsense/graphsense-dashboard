module Sub.Pathfinder exposing (subscriptions)

import Browser.Events
import Hovercard
import Json.Decode as Decode
import Model.Graph exposing (Dragging(..))
import Model.Pathfinder exposing (Model)
import Msg.Pathfinder exposing (Msg(..))
import Set
import Sub.Graph.Transform as Transform
import Time


keyDecoder : (String -> Msg) -> Decode.Decoder Msg
keyDecoder kMap =
    Decode.map kMap (Decode.field "key" Decode.string)


toKeyDown : String -> Msg
toKeyDown keyValue =
    case keyValue of
        "Control" ->
            -- https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values
            UserPressedCtrlKey

        "z" ->
            UserPressedNormalKey keyValue

        "y" ->
            UserPressedNormalKey keyValue

        _ ->
            NoOp


toKeyUp : String -> Msg
toKeyUp keyValue =
    case keyValue of
        "Control" ->
            -- https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values
            UserReleasedCtrlKey

        "Backspace" ->
            UserReleasedDeleteKey

        "z" ->
            UserReleasedNormalKey keyValue

        "y" ->
            UserReleasedNormalKey keyValue

        "Delete" ->
            -- https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values
            UserReleasedDeleteKey

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
    , Time.every 60000 Tick
    , if Set.isEmpty model.network.animatedAddresses && Set.isEmpty model.network.animatedTxs then
        Sub.none

      else
        Browser.Events.onAnimationFrameDelta AnimationFrameDeltaForMove
    , model.tooltip
        |> Maybe.map (.hovercard >> Hovercard.subscriptions >> Sub.map HovercardMsg)
        |> Maybe.withDefault Sub.none
    ]
        |> Sub.batch

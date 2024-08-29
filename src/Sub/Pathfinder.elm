module Sub.Pathfinder exposing (subscriptions)

import Browser.Events
import Hovercard
import Json.Decode as Decode
import Model.Graph exposing (Dragging(..))
import Model.Pathfinder exposing (Model)
import Msg.Pathfinder exposing (DisplaySettingsMsg(..), Msg(..))
import Set
import Sub.Graph.Transform as Transform
import Time


keyDecoder : (String -> Decode.Decoder Msg) -> Decode.Decoder Msg
keyDecoder kMap =
    Decode.andThen kMap (Decode.field "key" Decode.string)


toKeyDown : String -> Decode.Decoder Msg
toKeyDown keyValue =
    case keyValue of
        "Control" ->
            -- https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values
            Decode.succeed UserPressedCtrlKey

        "z" ->
            UserPressedNormalKey keyValue
                |> Decode.succeed

        "y" ->
            UserPressedNormalKey keyValue
                |> Decode.succeed

        _ ->
            NoOp
                |> Decode.succeed


toKeyUp : String -> Decode.Decoder Msg
toKeyUp keyValue =
    let
        decodeDeleteOnInput =
            Decode.at [ "target", "nodeName" ] Decode.string
                |> Decode.andThen
                    (\nodeName ->
                        if nodeName == "INPUT" then
                            Decode.fail "on input"

                        else
                            Decode.succeed UserReleasedDeleteKey
                    )
    in
    case keyValue of
        "Control" ->
            -- https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values
            UserReleasedCtrlKey
                |> Decode.succeed

        "z" ->
            UserReleasedNormalKey keyValue
                |> Decode.succeed

        "y" ->
            UserReleasedNormalKey keyValue
                |> Decode.succeed

        "Backspace" ->
            decodeDeleteOnInput

        "Delete" ->
            -- https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values
            decodeDeleteOnInput

        "Escape" ->
            UserReleasedEscape
                |> Decode.succeed

        _ ->
            NoOp
                |> Decode.succeed


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
    , model.config.displaySettingsHovercard
        |> Maybe.map (Hovercard.subscriptions >> Sub.map (DisplaySettingsHovercardMsg >> ChangedDisplaySettingsMsg))
        |> Maybe.withDefault Sub.none
    ]
        |> Sub.batch

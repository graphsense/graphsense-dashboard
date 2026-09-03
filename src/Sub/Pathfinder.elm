module Sub.Pathfinder exposing (subscriptions)

import Browser.Events
import Components.Tooltip as Tooltip
import Components.TransactionFilter as TransactionFilter
import Hovercard
import Json.Decode as Decode
import Model.Direction exposing (Direction(..))
import Model.Graph exposing (Dragging(..))
import Model.Pathfinder exposing (Details(..), Model)
import Msg.ExportDialog exposing (Msg(..))
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails
import Msg.Pathfinder.RelationDetails
import Msg.Pathfinder.TxDetails
import Ports
import RemoteData
import Set
import Sub.Graph.Transform as Transform


type alias KeyEvent =
    { key : String

    -- Ctrl or Cmd held down *according to this very event*, rather than tracked
    -- across separate keydown/keyup messages. See toKeyDown.
    , modHeld : Bool
    , repeat : Bool
    }


keyDecoder : (KeyEvent -> Decode.Decoder Msg) -> Decode.Decoder Msg
keyDecoder kMap =
    Decode.map3 KeyEvent
        (Decode.field "key" Decode.string)
        (Decode.map2 (||)
            (Decode.field "ctrlKey" Decode.bool)
            (Decode.field "metaKey" Decode.bool)
        )
        (Decode.field "repeat" Decode.bool)
        |> Decode.andThen kMap


onlyFireOutsideOfTextInput : Msg -> Decode.Decoder Msg
onlyFireOutsideOfTextInput msg =
    Decode.at [ "target", "nodeName" ] Decode.string
        |> Decode.andThen
            (\nodeName ->
                if nodeName == "INPUT" then
                    Decode.fail "on input"

                else if nodeName == "TEXTAREA" then
                    Decode.fail "on textarea"

                else
                    Decode.succeed msg
            )


{-| Mod-key chords (Ctrl/Cmd + key) fire on **keydown**, and read the modifier
straight off the event via `modHeld`.

Do not go back to firing them on keyup and gating them on a `modPressed` flag
kept in the model: that made the chord depend on the release order, so lifting
Ctrl a few milliseconds before the letter silently swallowed the shortcut. It
also never worked reliably on macOS, where browsers do not deliver keyup for
character keys while Cmd is held.

Keydown repeats are dropped so that holding a chord down does not, say, fire a
save per repeat.

-}
toKeyDown : KeyEvent -> Decode.Decoder Msg
toKeyDown { key, modHeld, repeat } =
    case key of
        -- https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values
        "Control" ->
            Decode.succeed UserPressedModKey

        "Meta" ->
            Decode.succeed UserPressedModKey

        "ArrowLeft" ->
            UserPressedArrowKey Incoming |> onlyFireOutsideOfTextInput

        "ArrowRight" ->
            UserPressedArrowKey Outgoing |> onlyFireOutsideOfTextInput

        "ArrowUp" ->
            UserPressedArrowKeyUp |> onlyFireOutsideOfTextInput

        "ArrowDown" ->
            UserPressedArrowKeyDown |> onlyFireOutsideOfTextInput

        _ ->
            if not modHeld || repeat then
                Decode.fail "not handled"

            else
                case key of
                    -- these have a meaning of their own inside a text input
                    -- (select all, undo, redo), so leave them to the browser there
                    "a" ->
                        UserPressedHotkey key |> onlyFireOutsideOfTextInput

                    "z" ->
                        UserPressedHotkey key |> onlyFireOutsideOfTextInput

                    "y" ->
                        UserPressedHotkey key |> onlyFireOutsideOfTextInput

                    "f" ->
                        UserPressedHotkey key |> Decode.succeed

                    "k" ->
                        UserPressedHotkey key |> Decode.succeed

                    "s" ->
                        UserPressedHotkey key |> Decode.succeed

                    "e" ->
                        UserPressedHotkey key |> Decode.succeed

                    _ ->
                        Decode.fail "not handled"


toKeyUp : KeyEvent -> Decode.Decoder Msg
toKeyUp { key } =
    case key of
        -- https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values
        "Control" ->
            UserReleasedModKey
                |> Decode.succeed

        "Meta" ->
            UserReleasedModKey
                |> Decode.succeed

        "Backspace" ->
            UserReleasedDeleteKey |> onlyFireOutsideOfTextInput

        "Delete" ->
            -- https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values
            UserReleasedDeleteKey |> onlyFireOutsideOfTextInput

        "Escape" ->
            UserReleasedEscape
                |> Decode.succeed

        _ ->
            Decode.fail "not handled"


subscriptions : Model -> Sub Msg
subscriptions model =
    [ case ( model.dragging, model.draggingAggEdgeLabel ) of
        ( NoDragging, Nothing ) ->
            Sub.none

        _ ->
            Browser.Events.onMouseUp (Decode.succeed UserReleasesMouseButton)
    , Transform.subscriptions AnimationFrameDeltaForTransform model.transform

    -- All shortcuts are handled here; for those with a browser default (Ctrl+F,
    -- Ctrl+S, ...) preventDefault() is called on the same keydown in main.js.
    , Browser.Events.onKeyDown (keyDecoder toKeyDown)
    , Browser.Events.onKeyUp (keyDecoder toKeyUp)
    , Browser.Events.onVisibilityChange (\_ -> UserReleasedModKey)
    , Ports.windowBlurred (\_ -> UserReleasedModKey)

    -- , Time.every 60000 Tick
    , if Set.isEmpty model.network.animatedAddresses && Set.isEmpty model.network.animatedTxs then
        Sub.none

      else
        Browser.Events.onAnimationFrameDelta AnimationFrameDeltaForMove
    , model.toolbarHovercard
        |> Maybe.map (Tuple.second >> Hovercard.subscriptions >> Sub.map ToolbarHovercardMsg)
        |> Maybe.withDefault Sub.none
    , case model.details of
        Just (TxDetails _ txDetailsModel) ->
            TransactionFilter.subscriptions txDetailsModel.subTxsTableFilter
                |> Sub.map (\msg -> TxDetailsMsg (Msg.Pathfinder.TxDetails.TransactionFilterMsg msg))

        Just (AddressDetails aid addressDetailsModel) ->
            case addressDetailsModel.txs of
                RemoteData.Success txsModel ->
                    TransactionFilter.subscriptions txsModel.filter
                        |> Sub.map (\msg -> AddressDetailsMsg aid (Msg.Pathfinder.AddressDetails.TransactionFilterMsg msg))

                _ ->
                    Sub.none

        Just (RelationDetails rid relationDetailsModel) ->
            let
                a2bFilterSub =
                    if relationDetailsModel.a2bTableOpen then
                        TransactionFilter.subscriptions relationDetailsModel.a2bTable.filter
                            |> Sub.map (\msg -> RelationDetailsMsg rid (Msg.Pathfinder.RelationDetails.TransactionFilterMsg True msg))

                    else
                        Sub.none

                b2aFilterSub =
                    if relationDetailsModel.b2aTableOpen then
                        TransactionFilter.subscriptions relationDetailsModel.b2aTable.filter
                            |> Sub.map (\msg -> RelationDetailsMsg rid (Msg.Pathfinder.RelationDetails.TransactionFilterMsg False msg))

                    else
                        Sub.none
            in
            Sub.batch [ a2bFilterSub, b2aFilterSub ]

        _ ->
            Sub.none
    , Tooltip.subscriptions model.tooltip
        |> Sub.map TooltipMsg
    ]
        |> Sub.batch

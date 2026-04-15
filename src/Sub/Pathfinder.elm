module Sub.Pathfinder exposing (subscriptions)

import Browser.Events
import Components.TransactionFilter as TransactionFilter
import Hovercard
import Json.Decode as Decode
import Model.Graph exposing (Dragging(..))
import Model.Pathfinder exposing (Details(..), Model)
import Msg.ExportDialog exposing (Msg(..))
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails
import Msg.Pathfinder.RelationDetails
import Msg.Pathfinder.TxDetails
import RemoteData
import Set
import Sub.Graph.Transform as Transform


keyDecoder : (String -> Decode.Decoder Msg) -> Decode.Decoder Msg
keyDecoder kMap =
    Decode.andThen kMap (Decode.field "key" Decode.string)


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


toKeyDown : String -> Decode.Decoder Msg
toKeyDown keyValue =
    case keyValue of
        -- https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values
        "Control" ->
            Decode.succeed UserPressedModKey

        "Meta" ->
            Decode.succeed UserPressedModKey

        "Shift" ->
            Decode.succeed UserPressedModKey

        "z" ->
            UserPressedNormalKey keyValue |> onlyFireOutsideOfTextInput

        "y" ->
            UserPressedNormalKey keyValue |> onlyFireOutsideOfTextInput

        _ ->
            Decode.fail "not handled"


toKeyUp : String -> Decode.Decoder Msg
toKeyUp keyValue =
    case keyValue of
        -- https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values
        "Control" ->
            UserReleasedModKey
                |> Decode.succeed

        "Meta" ->
            UserReleasedModKey
                |> Decode.succeed

        "Shift" ->
            UserReleasedModKey |> Decode.succeed

        "z" ->
            UserReleasedNormalKey keyValue |> onlyFireOutsideOfTextInput

        "y" ->
            UserReleasedNormalKey keyValue |> onlyFireOutsideOfTextInput

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
    [ case model.dragging of
        NoDragging ->
            Sub.none

        _ ->
            Browser.Events.onMouseUp (Decode.succeed UserReleasesMouseButton)
    , Transform.subscriptions AnimationFrameDeltaForTransform model.transform
    , Browser.Events.onKeyDown (keyDecoder toKeyDown)
    , Browser.Events.onKeyUp (keyDecoder toKeyUp)
    , Browser.Events.onVisibilityChange (\_ -> UserReleasedModKey)

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
    ]
        |> Sub.batch

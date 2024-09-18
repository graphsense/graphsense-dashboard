module Update.Notification exposing (notificationsFromEffects)

import Effect.Pathfinder as Pathfinder
import Model
import Model.Notification exposing (..)
import Model.Pathfinder.Error exposing (Error(..), InternalError(..))
import RecordSetter exposing (..)
import Tuple exposing (..)


notificationsFromEffects : Model.Model key -> List Model.Effect -> ( Model.Model key, List Model.Effect )
notificationsFromEffects model effects =
    let
        notifications =
            effects |> List.filterMap (notificationFromEffect model) |> List.concatMap identity

        nm =
            model.notifications |> s_messages (List.append model.notifications.messages notifications)
    in
    ( model |> s_notifications nm, effects )


notificationFromEffect : Model.Model key -> Model.Effect -> Maybe (List Notification)
notificationFromEffect _ effect =
    case effect of
        Model.PathfinderEffect (Pathfinder.ErrorEffect x) ->
            Just (errorToNotifications x)

        _ ->
            Nothing


errorToNotifications : Error -> List Notification
errorToNotifications err =
    case err of
        InternalError (AddressNotFoundInDict _) ->
            Error "address not found" "Address Not found" |> List.singleton

        InternalError (TxValuesEmpty _ _) ->
            Error "tx not found" "Address Not found" |> List.singleton

        InternalError (NoTxInputsOutputsFoundInDict _) ->
            Error "tx io not found" "tx io Not found" |> List.singleton

        Errors x ->
            x |> List.map errorToNotifications |> List.concatMap identity



-- update : String -> Maybe Http.Error -> Model -> Model
-- update key error model =
--     Dict.get key model.messages
--         |> Maybe.map
--             (\msg ->
--                 { model
--                     | messages = Dict.remove key model.messages
--                     , log = addLog ( first msg, second msg, error ) model.log
--                     , visible =
--                         if first msg == loadingAddressKey then
--                             model.visible
--                         else if first msg == loadingAddressEntityKey then
--                             model.visible
--                         else
--                             error
--                                 |> Maybe.map (\_ -> True)
--                                 |> Maybe.withDefault model.visible
--                 }
--             )
--         |> Maybe.withDefault model
-- add : Model -> String -> List String -> Maybe Http.Error -> Model
-- add model key values error =
--     { model
--         | log = ( key, values, error ) :: model.log
--         , visible =
--             error
--                 |> Maybe.map (\_ -> True)
--                 |> Maybe.withDefault model.visible
--     }

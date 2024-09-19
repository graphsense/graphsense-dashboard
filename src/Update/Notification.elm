module Update.Notification exposing (addHttpError, notificationsFromEffects)

import Basics.Extra exposing (flip)
import Effect.Pathfinder as Pathfinder
import Http
import Model
import Model.Notification as Notify
import Model.Pathfinder.Error exposing (Error(..), InternalError(..))
import RecordSetter exposing (..)
import Tuple exposing (..)


notificationsFromEffects : Model.Model key -> List Model.Effect -> ( Model.Model key, List Model.Effect )
notificationsFromEffects model effects =
    let
        notifications =
            effects |> List.filterMap (notificationFromEffect model) |> List.concatMap identity
    in
    ( model |> s_notifications (model.notifications |> flip Notify.addMany notifications), effects )


notificationFromEffect : Model.Model key -> Model.Effect -> Maybe (List Notify.Notification)
notificationFromEffect _ effect =
    case effect of
        Model.PathfinderEffect (Pathfinder.ErrorEffect x) ->
            Just (pathFinderErrorToNotifications x)

        _ ->
            Nothing


pathFinderErrorToNotifications : Error -> List Notify.Notification
pathFinderErrorToNotifications err =
    case err of
        InternalError (AddressNotFoundInDict _) ->
            Notify.Error { title = "Not Found", message = "Address not found", variables = [] } |> List.singleton

        InternalError (TxValuesEmpty _ _) ->
            Notify.Error { title = "Not Found", message = "Address not found", variables = [] } |> List.singleton

        InternalError (NoTxInputsOutputsFoundInDict _) ->
            Notify.Error { title = "Not Found", message = "Address not found", variables = [] } |> List.singleton

        Errors x ->
            x |> List.map pathFinderErrorToNotifications |> List.concatMap identity


addHttpError : Notify.Model -> Maybe String -> Http.Error -> Notify.Model
addHttpError m _ error =
    let
        nn =
            case error of
                Http.NetworkError ->
                    Notify.Error { title = "Network Issue", message = "There is no network connection...", variables = [] }

                Http.BadBody _ ->
                    Notify.Error { title = "Data Error", message = "There was a problem while loading data.", variables = [] }

                Http.BadUrl _ ->
                    Notify.Error { title = "Request Error", message = "There was a problem while loading data.", variables = [] }

                Http.BadStatus _ ->
                    Notify.Error { title = "Request Error", message = "There was a problem while loading data.", variables = [] }

                Http.Timeout ->
                    Notify.Error { title = "Request Timeout", message = "There was a problem while loading data.", variables = [] }
    in
    m |> flip Notify.add nn

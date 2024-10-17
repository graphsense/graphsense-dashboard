module Update.Notification exposing (addHttpError, notificationsFromEffects)

import Basics.Extra exposing (flip)
import Effect.Pathfinder as Pathfinder
import Http
import Model exposing (Model)
import Model.Notification as Notify
import Model.Pathfinder.Error exposing (Error(..), InfoError(..), InternalError(..))
import Model.Pathfinder.Id as Id
import RecordSetter exposing (..)
import Util.View exposing (truncateLongIdentifierWithLengths)


notificationsFromEffects : Model key -> List Model.Effect -> ( Model key, List Model.Effect )
notificationsFromEffects model effects =
    let
        notifications =
            effects |> List.filterMap (notificationFromEffect model) |> List.concat
    in
    ( model |> s_notifications (model.notifications |> flip Notify.addMany notifications), effects )


notificationFromEffect : Model key -> Model.Effect -> Maybe (List Notify.Notification)
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

        InfoError (NoAdjaccentTxForAddressFound tid) ->
            Notify.Info
                { title = "Transaction tracing not possible"
                , message = "Could not find a suitable adjacent transaction for address {0}. This is likely because the funds are not yet spent."
                , variables =
                    Id.id tid |> List.singleton
                }
                |> List.singleton

        InfoError (TxTracingThroughService id exchangeLabel) ->
            Notify.Info
                { title = "Transaction tracing not reasonable"
                , message =
                    exchangeLabel
                        |> Maybe.map
                            (\_ ->
                                "Since {0} belongs to an exchange service ({1}) automatic tracing of individual transactions is not reasonable. Please try to pick a transaction from the address's transaction list."
                            )
                        |> Maybe.withDefault "Since {0} seems to belong to a service automatic tracing of individual transactions is not reasonable. Please try to pick a transaction from the address's transaction list."
                , variables =
                    (Id.id id |> truncateLongIdentifierWithLengths 8 4)
                        :: (exchangeLabel
                                |> Maybe.map List.singleton
                                |> Maybe.withDefault []
                           )
                }
                |> List.singleton

        Errors x ->
            x |> List.concatMap pathFinderErrorToNotifications


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

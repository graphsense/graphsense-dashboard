module Update.Notification exposing (addHttpError, notificationsFromEffects)

import Effect.Pathfinder as Pathfinder
import Http
import Model exposing (Effect(..), Model)
import Model.Notification as Notify
import Model.Pathfinder.Error exposing (Error(..), InfoError(..), InternalError(..))
import Model.Pathfinder.Id as Id
import Util.View exposing (truncateLongIdentifierWithLengths)


notificationsFromEffects : Model key -> List Model.Effect -> ( Model key, List Model.Effect )
notificationsFromEffects model effects =
    let
        ( notifications, eff ) =
            effects
                |> List.filterMap (notificationFromEffect model)
                |> List.concat
                |> Notify.addMany model.notifications
    in
    ( { model | notifications = notifications }
    , (eff
        |> List.map NotificationEffect
      )
        ++ effects
    )


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
            Notify.Error { title = "Not Found", message = "Address not found", moreInfo = [], variables = [] } |> List.singleton

        InternalError (TxValuesEmpty _ _) ->
            Notify.Error { title = "Not Found", message = "Address not found", moreInfo = [], variables = [] } |> List.singleton

        InternalError (NoTxInputsOutputsFoundInDict _) ->
            Notify.Error { title = "Not Found", message = "Address not found", moreInfo = [], variables = [] } |> List.singleton

        InfoError (NoAdjaccentTxForAddressFound tid) ->
            Notify.Info
                { title = "Transaction tracing not possible"
                , message = "Could not find a suitable adjacent transaction for address {0}. This is likely because the funds are not yet spent."
                , moreInfo = []
                , variables =
                    Id.id tid |> List.singleton
                }
                |> List.singleton

        InfoError (TxTracingThroughService id exchangeLabel) ->
            Notify.Info
                { title = "Auto trace limit"
                , moreInfo = []
                , message =
                    "Auto tracing stops at service addresses, as asset flows typically cannot be traced through these services. This limitation occurs because services often act as black boxes, mixing user funds. You can still manually trace outgoing transactions using the tracing options available in the side panel."
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


addHttpError : Notify.Model -> Maybe String -> Http.Error -> ( Notify.Model, List Notify.Effect )
addHttpError m _ error =
    let
        nn =
            Notify.fromHttpError error
    in
    Notify.add nn m

module Update.Statusbar exposing (add, messagesFromEffects, toggle, update, updateLastBlocks)

import Api.Data
import Api.Request.Entities
import Dict
import Effect.Api as Api
import Effect.Graph as Graph
import Effect.Locale as Locale
import Effect.Pathfinder as Pathfinder
import Effect.Search as Search
import Http
import List.Extra
import Model
import Model.Statusbar exposing (..)
import Tuple exposing (..)
import View.Locale as Locale


messagesFromEffects : Model.Model key -> List Model.Effect -> ( Model.Model key, List ( Maybe String, Model.Effect ) )
messagesFromEffects model effects =
    effects
        |> List.foldl
            (\eff ( statusbar, newEffects ) ->
                messageFromEffect model eff
                    |> Maybe.map
                        (\( key, message ) ->
                            let
                                keyJoined =
                                    key ++ String.concat message
                            in
                            ( { statusbar
                                | messages = Dict.insert keyJoined ( key, message ) statusbar.messages
                              }
                            , ( Just keyJoined, eff ) :: newEffects
                            )
                        )
                    |> Maybe.withDefault ( statusbar, ( Nothing, eff ) :: newEffects )
            )
            ( model.statusbar, [] )
        |> mapFirst
            (\statusbar ->
                { model | statusbar = statusbar }
            )


messageFromEffect : Model.Model key -> Model.Effect -> Maybe ( String, List String )
messageFromEffect model effect =
    case effect of
        Model.NavLoadEffect _ ->
            Nothing

        Model.NavPushUrlEffect _ ->
            Nothing

        Model.NavBackEffect ->
            Nothing

        Model.GetElementEffect _ ->
            Nothing

        Model.GetContentsElementEffect ->
            Nothing

        Model.LocaleEffect (Locale.GetTranslationEffect _) ->
            Nothing

        Model.LocaleEffect (Locale.GetTimezoneEffect _) ->
            Nothing

        Model.SearchEffect (Search.SearchEffect _) ->
            Nothing

        Model.SearchEffect Search.CancelEffect ->
            Nothing

        Model.SearchEffect (Search.CmdEffect _) ->
            Nothing

        Model.PluginEffect _ ->
            Nothing

        Model.PortsConsoleEffect _ ->
            Nothing

        Model.CmdEffect _ ->
            Nothing

        Model.LogoutEffect ->
            Nothing

        Model.SetDirtyEffect ->
            Nothing

        Model.SaveUserSettingsEffect _ ->
            Nothing

        Model.SetCleanEffect ->
            Nothing

        Model.ApiEffect eff ->
            messageFromApiEffect model eff

        Model.GraphEffect (Graph.ApiEffect eff) ->
            messageFromApiEffect model eff

        Model.GraphEffect (Graph.NavPushRouteEffect _) ->
            Nothing

        Model.GraphEffect Graph.GetBrowserElementEffect ->
            Nothing

        Model.GraphEffect (Graph.PluginEffect _) ->
            Nothing

        Model.GraphEffect (Graph.InternalGraphAddedAddressesEffect _) ->
            Nothing

        Model.GraphEffect (Graph.InternalGraphAddedEntitiesEffect _) ->
            Nothing

        Model.GraphEffect (Graph.InternalGraphSelectedAddressEffect _) ->
            Nothing

        Model.GraphEffect (Graph.TagSearchEffect _) ->
            Nothing

        Model.GraphEffect (Graph.CmdEffect _) ->
            Nothing

        Model.GraphEffect (Graph.DownloadCSVEffect _) ->
            Nothing

        Model.PathfinderEffect (Pathfinder.ApiEffect eff) ->
            messageFromApiEffect model eff

        Model.PathfinderEffect (Pathfinder.CmdEffect _) ->
            Nothing

        Model.PathfinderEffect (Pathfinder.PluginEffect _) ->
            Nothing

        Model.PathfinderEffect (Pathfinder.NavPushRouteEffect _) ->
            Nothing

        Model.PathfinderEffect (Pathfinder.SearchEffect _) ->
            Nothing

        Model.PathfinderEffect (Pathfinder.ErrorEffect _) ->
            Nothing

        Model.PathfinderEffect (Pathfinder.PostponeUpdateByRouteEffect _) ->
            Nothing

        Model.PathfinderEffect (Pathfinder.ShowNotificationEffect _) ->
            Nothing

        Model.NotificationEffect _ ->
            Nothing

        Model.PostponeUpdateByUrlEffect _ ->
            Nothing

        Model.PathfinderEffect (Pathfinder.OpenTooltipEffect _ _) ->
            Nothing

        Model.PathfinderEffect (Pathfinder.CloseTooltipEffect _ _) ->
            Nothing

        Model.PathfinderEffect Pathfinder.RepositionTooltipEffect ->
            Nothing

        Model.PathfinderEffect (Pathfinder.InternalEffect _) ->
            Nothing


isOutgoingToString : Bool -> String
isOutgoingToString isOutgoing =
    if isOutgoing then
        "outgoing"

    else
        "incoming"


isOutputToString : Bool -> String
isOutputToString isOutgoing =
    if isOutgoing then
        "output"

    else
        "input"


update : String -> Maybe Http.Error -> Model -> Model
update key error model =
    Dict.get key model.messages
        |> Maybe.map
            (\msg ->
                { model
                    | messages = Dict.remove key model.messages
                    , log = addLog ( first msg, second msg, error ) model.log
                }
            )
        |> Maybe.withDefault model


updateLastBlocks : Api.Data.Stats -> Model -> Model
updateLastBlocks stats model =
    { model
        | lastBlocks =
            stats.currencies
                |> List.map (\{ name, noBlocks } -> ( name, noBlocks - 1 ))
    }


toggle : Model -> Model
toggle model =
    { model | visible = not model.visible }


add : Model -> String -> List String -> Maybe Http.Error -> Model
add model key values error =
    { model
        | log = ( key, values, error ) :: model.log

        -- , visible =
        --     error
        --         |> Maybe.map (\_ -> True)
        --         |> Maybe.withDefault model.visible
    }


messageFromApiEffect : Model.Model key -> Api.Effect msg -> Maybe ( String, List String )
messageFromApiEffect model effect =
    case effect of
        Api.GetConceptsEffect taxonomy _ ->
            ( "loading concepts for taxonomy {0}"
            , [ taxonomy ]
            )
                |> Just

        Api.ListSupportedTokensEffect currency _ ->
            ( "loading supported token currencies for " ++ currency
            , []
            )
                |> Just

        Api.SearchEffect _ _ ->
            Nothing

        Api.GetAddressTagSummaryEffect _ _ ->
            Nothing

        Api.GetStatisticsEffect _ ->
            Nothing

        Api.GetBlockByDateEffect _ _ ->
            Nothing

        Api.SearchEntityNeighborsEffect e _ ->
            ( searchNeighborsKey
            , [ if e.isOutgoing then
                    "for outgoing neighbors"

                else
                    "for incoming neighbors"
              , e.entity |> String.fromInt
              , case e.key of
                    Api.Request.Entities.KeyCategory ->
                        e.value
                            |> List.head
                            |> Maybe.map
                                (\cat ->
                                    List.Extra.find (.id >> (==) cat) model.config.allConcepts
                                        |> Maybe.map .label
                                        |> Maybe.withDefault cat
                                )
                            |> Maybe.withDefault ""
                            |> (\s -> Locale.string model.config.locale "category" ++ " " ++ s)

                    _ ->
                        ""
              , String.fromInt e.depth
              , String.fromInt e.breadth
              , String.fromInt e.maxAddresses
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.GetActorEffect e _ ->
            ( loadingActorKey
            , [ e.actorId ]
            )
                |> Just

        Api.GetActorTagsEffect e _ ->
            ( loadingActorTagsKey
            , [ e.actorId ]
            )
                |> Just

        Api.GetAddressEffect e _ ->
            ( loadingAddressKey
            , [ e.address
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.GetEntityForAddressEffect e _ ->
            ( loadingAddressEntityKey
            , [ e.address
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.GetEntityEffect e _ ->
            ( "{1}: loading entity {0}"
            , [ String.fromInt e.entity
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.GetEntityEffectWithDetails e _ ->
            ( "{1}: loading entity {0}"
            , [ String.fromInt e.entity
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.GetBlockEffect e _ ->
            ( "{1}: loading block {0}"
            , [ String.fromInt e.height
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.GetTxEffect e _ ->
            ( "{1}: loading transactions {0}"
            , [ e.txHash
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.GetTxUtxoAddressesEffect e _ ->
            ( "{1}: loading " ++ isOutputToString e.isOutgoing ++ " addresses of transaction {0}"
            , [ e.txHash
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.ListSpendingTxRefsEffect e _ ->
            ( "{1}: loading transactions which {0} is spending"
            , [ e.txHash
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.ListSpentInTxRefsEffect e _ ->
            ( "{1}: loading transactions where {0} got spent"
            , [ e.txHash
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.GetAddressNeighborsEffect e _ ->
            ( "{1}: loading " ++ isOutgoingToString e.isOutgoing ++ " neighbors of address {0}"
            , [ e.address
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.GetEntityNeighborsEffect e _ ->
            ( "{1}: loading " ++ isOutgoingToString e.isOutgoing ++ " neighbors of entity {0}"
            , [ e.entity |> String.fromInt
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.GetAddressTagsEffect e _ ->
            ( "{1}: loading tags of address {0}"
            , [ e.address
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.GetEntityAddressTagsEffect e _ ->
            ( "{1}: loading address tags of entity {0}"
            , [ String.fromInt e.entity
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.GetAddressTxsEffect e _ ->
            ( "{1}: loading transactions of address {0}"
            , [ e.address
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.GetEntityTxsEffect e _ ->
            ( "{1}: loading transactions of entity {0}"
            , [ String.fromInt e.entity
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.GetBlockTxsEffect e _ ->
            ( "{1}: loading transactions of block {0}"
            , [ String.fromInt e.block
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.GetTokenTxsEffect e _ ->
            ( "{1}: loading token transactions of transaction {0}"
            , [ e.txHash
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.GetEntityAddressesEffect e _ ->
            ( "{1}: loading addresses of entity {0}"
            , [ String.fromInt e.entity
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.BulkGetAddressTagSummaryEffect e _ ->
            ( "{1}: loading {0} tag summaries"
            , [ List.length e.addresses |> String.fromInt
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.BulkGetAddressEffect e _ ->
            ( "{1}: loading {0} addresses"
            , [ List.length e.addresses |> String.fromInt
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.BulkGetEntityEffect e _ ->
            ( "{1}: loading {0} entities"
            , [ List.length e.entities |> String.fromInt
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.BulkGetAddressEntityEffect e _ ->
            ( "{1}: loading entities of {0} addresses"
            , [ List.length e.addresses |> String.fromInt
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.BulkGetEntityNeighborsEffect e _ ->
            ( "{1}: loading " ++ isOutgoingToString e.isOutgoing ++ " neighbors of {0} entities"
            , [ List.length e.entities |> String.fromInt
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.BulkGetAddressNeighborsEffect e _ ->
            ( "{1}: loading " ++ isOutgoingToString e.isOutgoing ++ " neighbors of {0} addresses"
            , [ List.length e.addresses |> String.fromInt
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.ListAddressTagsEffect e _ ->
            ( "Loading tags with label {0}"
            , [ e.label ]
            )
                |> Just

        Api.GetAddresslinkTxsEffect e _ ->
            ( "{2}: loading address link transactions between {0} and {1}"
            , [ e.source
              , e.target
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.GetEntitylinkTxsEffect e _ ->
            ( "{2}: loading entity link transactions between {0} and {1}"
            , [ String.fromInt e.source
              , String.fromInt e.target
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.BulkGetAddressTagsEffect e _ ->
            ( "{1}: loading tags of {0} addresses"
            , [ List.length e.addresses |> String.fromInt
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Api.BulkGetTxEffect e _ ->
            ( "{1}: loading {0} transactions"
            , [ List.length e.txs |> String.fromInt
              , e.currency |> String.toUpper
              ]
            )
                |> Just


addLog : ( String, List String, Maybe Http.Error ) -> List ( String, List String, Maybe Http.Error ) -> List ( String, List String, Maybe Http.Error )
addLog ( key, values, error ) logs =
    ( key, values, error )
        :: logs
        |> (if error == Just (Http.BadStatus 404) && (key == loadingAddressKey || key == loadingAddressEntityKey) then
                removeEntityNotFoundErrors values

            else
                identity
           )


removeEntityNotFoundErrors : List String -> List ( String, List String, Maybe Http.Error ) -> List ( String, List String, Maybe Http.Error )
removeEntityNotFoundErrors values messages =
    (List.take 10 messages
        |> List.filter ((/=) ( loadingAddressEntityKey, values, Just (Http.BadStatus 404) ))
    )
        ++ List.drop 10 messages

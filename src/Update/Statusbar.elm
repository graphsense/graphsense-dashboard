module Update.Statusbar exposing (..)

import Api.Request.Entities
import Dict
import Effect.Graph as Graph
import Effect.Locale as Locale
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
                                    key ++ String.join "" message
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

        Model.GetStatisticsEffect ->
            Nothing

        Model.GetConceptsEffect taxonomy _ ->
            ( "loading concepts for taxonomy {0}"
            , [ taxonomy ]
            )
                |> Just

        Model.GetElementEffect _ ->
            Nothing

        Model.LocaleEffect (Locale.GetTranslationEffect _) ->
            Nothing

        Model.LocaleEffect (Locale.GetTimezoneEffect _) ->
            Nothing

        Model.SearchEffect (Search.SearchEffect { query }) ->
            Nothing

        Model.SearchEffect Search.CancelEffect ->
            Nothing

        Model.SearchEffect (Search.BounceEffect _ _) ->
            Nothing

        Model.PluginEffect _ ->
            Nothing

        Model.PortsConsoleEffect _ ->
            Nothing

        Model.CmdEffect _ ->
            Nothing

        Model.LogoutEffect ->
            Nothing

        Model.GraphEffect (Graph.SearchEntityNeighborsEffect e) ->
            ( searchNeighborsKey
            , [ case e.isOutgoing of
                    False ->
                        "for incoming neighbors"

                    True ->
                        "for outgoing neighbors"
              , e.entity |> String.fromInt
              , case e.key of
                    Api.Request.Entities.KeyCategory ->
                        e.value
                            |> List.head
                            |> Maybe.map
                                (\cat ->
                                    List.Extra.find (.id >> (==) cat) model.graph.config.entityConcepts
                                        |> Maybe.map .label
                                        |> Maybe.withDefault cat
                                )
                            |> Maybe.withDefault ""
                            |> (\s -> Locale.string model.locale "category" ++ " " ++ s)

                    _ ->
                        ""
              , String.fromInt e.depth
              , String.fromInt e.breadth
              , String.fromInt e.maxAddresses
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.GetAddressEffect e) ->
            ( "{1}: loading address {0}"
            , [ e.address
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.GetEntityForAddressEffect e) ->
            ( "{1}: loading entity for address {0}"
            , [ e.address
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.GetEntityEffect e) ->
            ( "{1}: loading entity {0}"
            , [ String.fromInt e.entity
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.GetBlockEffect e) ->
            ( "{1}: loading block {0}"
            , [ String.fromInt e.height
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.GetTxEffect e) ->
            ( "{1}: loading transactions {0}"
            , [ e.txHash
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.GetTxUtxoAddressesEffect e) ->
            ( "{1}: loading " ++ isOutputToString e.isOutgoing ++ " addresses of transaction {0}"
            , [ e.txHash
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.GetAddressNeighborsEffect e) ->
            ( "{1}: loading " ++ isOutgoingToString e.isOutgoing ++ " neighbors of address {0}"
            , [ e.address
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.GetEntityNeighborsEffect e) ->
            ( "{1}: loading " ++ isOutgoingToString e.isOutgoing ++ " neighbors of entity {0}"
            , [ e.entity |> String.fromInt
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.GetAddressTagsEffect e) ->
            ( "{1}: loading tags of address {0}"
            , [ e.address
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.GetEntityAddressTagsEffect e) ->
            ( "{1}: loading address tags of entity {0}"
            , [ String.fromInt e.entity
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.GetAddressTxsEffect e) ->
            ( "{1}: loading transactions of address {0}"
            , [ e.address
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.GetEntityTxsEffect e) ->
            ( "{1}: loading transactions of entity {0}"
            , [ String.fromInt e.entity
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.GetBlockTxsEffect e) ->
            ( "{1}: loading transactions of block {0}"
            , [ String.fromInt e.block
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.GetEntityAddressesEffect e) ->
            ( "{1}: loading addresses of entity {0}"
            , [ String.fromInt e.entity
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.BulkGetAddressEffect e) ->
            ( "{1}: loading {0} addresses"
            , [ List.length e.addresses |> String.fromInt
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.BulkGetEntityEffect e) ->
            ( "{1}: loading {0} entities"
            , [ List.length e.entities |> String.fromInt
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.BulkGetAddressEntityEffect e) ->
            ( "{1}: loading entities of {0} addresses"
            , [ List.length e.addresses |> String.fromInt
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.BulkGetEntityNeighborsEffect e) ->
            ( "{1}: loading " ++ isOutgoingToString e.isOutgoing ++ " neighbors of {0} entities"
            , [ List.length e.entities |> String.fromInt
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.NavPushRouteEffect _) ->
            Nothing

        Model.GraphEffect Graph.GetSvgElementEffect ->
            Nothing

        Model.GraphEffect Graph.GetBrowserElementEffect ->
            Nothing

        Model.GraphEffect (Graph.ListAddressTagsEffect e) ->
            ( "{1}: loading tags with label {0}"
            , [ e.label ]
            )
                |> Just

        Model.GraphEffect (Graph.GetAddresslinkTxsEffect e) ->
            ( "{2}: loading address link transactions between {0} and {1}"
            , [ e.source
              , e.target
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.GetEntitylinkTxsEffect e) ->
            ( "{2}: loading entity link transactions between {0} and {1}"
            , [ String.fromInt e.source
              , String.fromInt e.target
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.BulkGetAddressTagsEffect e) ->
            ( "{1}: loading tags of {0} addresses"
            , [ List.length e.addresses |> String.fromInt
              , e.currency |> String.toUpper
              ]
            )
                |> Just

        Model.GraphEffect (Graph.PluginEffect _) ->
            Nothing

        Model.GraphEffect (Graph.InternalGraphAddedAddressesEffect _) ->
            Nothing

        Model.GraphEffect (Graph.InternalGraphAddedEntitiesEffect _) ->
            Nothing

        Model.GraphEffect (Graph.TagSearchEffect _) ->
            Nothing

        Model.GraphEffect (Graph.CmdEffect _) ->
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


update : Bool -> String -> Maybe Http.Error -> Model -> Model
update hide key error model =
    Dict.get key model.messages
        |> Maybe.map
            (\msg ->
                { model
                    | messages = Dict.remove key model.messages
                    , log = ( first msg, second msg, error ) :: model.log
                    , visible =
                        error
                            |> Maybe.map (\_ -> not hide)
                            |> Maybe.withDefault model.visible
                }
            )
        |> Maybe.withDefault model


toggle : Model -> Model
toggle model =
    { model | visible = not model.visible }


add : Model -> String -> List String -> Maybe Http.Error -> Model
add model key values error =
    { model
        | log = ( key, values, error ) :: model.log
        , visible =
            error
                |> Maybe.map (\_ -> True)
                |> Maybe.withDefault model.visible
    }

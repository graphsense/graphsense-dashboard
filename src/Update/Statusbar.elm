module Update.Statusbar exposing (..)

import Api.Request.Entities
import Dict
import Effect.Graph as Graph
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
                            ( { statusbar
                                | messages = Dict.insert key message statusbar.messages
                              }
                            , ( Just key, eff ) :: newEffects
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
        Model.GraphEffect (Graph.SearchEntityNeighborsEffect e) ->
            ( "searching {0} of {1} with {2} (depth: {3}, breadth: {4}, skip if more than {5} addresses)"
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
                                    List.Extra.find (.id >> (==) cat) model.graph.entityConcepts
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
              ]
            )
                |> Just

        Model.GraphEffect (Graph.GetAddressNeighborsEffect e) ->
            ( "loading " ++ isOutgoingToString e.isOutgoing ++ " neighbors of address {0}"
            , [ e.address ]
            )
                |> Just

        Model.GraphEffect (Graph.GetEntityNeighborsEffect e) ->
            ( "loading " ++ isOutgoingToString e.isOutgoing ++ " neighbors of entity {0}"
            , [ e.entity |> String.fromInt ]
            )
                |> Just

        _ ->
            Nothing


isOutgoingToString : Bool -> String
isOutgoingToString isOutgoing =
    if isOutgoing then
        "outgoing"

    else
        "incoming"


removeMessage : String -> Model -> Model
removeMessage key model =
    { model
        | messages = Dict.remove key model.messages
    }

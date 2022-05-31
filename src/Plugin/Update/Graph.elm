module Plugin.Update.Graph exposing (..)

import Dict
import Json.Encode exposing (Value)
import Model.Graph.Id as Id
import Plugin exposing (Plugins)
import Plugin.Model exposing (..)
import Set exposing (Set)
import Tuple exposing (..)


addressesAdded : Plugins -> PluginStates -> Set Id.AddressId -> ( PluginStates, List (OutMsg Value), List ( String, Cmd Value ) )
addressesAdded plugins states new =
    plugins
        |> Dict.toList
        |> List.foldl
            (\( pid, plugin ) ( states_, outMsgs, cmds ) ->
                Dict.get pid states_
                    |> Maybe.map
                        (\state ->
                            let
                                ( newState, outMsg, cmd ) =
                                    plugin.update.graph.addressesAdded new state
                            in
                            ( Dict.insert pid newState states_
                            , outMsgs ++ outMsg
                            , ( pid, cmd ) :: cmds
                            )
                        )
                    |> Maybe.withDefault ( states_, outMsgs, cmds )
            )
            ( states, [], [] )

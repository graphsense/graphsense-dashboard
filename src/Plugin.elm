module Plugin exposing (..)

import Config.View as View
import Dict exposing (Dict)
import Html.Styled as Html exposing (..)
import Json.Encode exposing (Value)
import Model.Graph.Id as Id
import Plugin.Model exposing (..)
import Svg.Styled as Svg exposing (..)


type alias Plugins =
    Dict String Plugin


type alias Plugin =
    { view :
        { graph :
            { address :
                { flags : View.Config -> Id.AddressId -> Maybe Value -> Maybe (Svg Value)
                }
            , navbar :
                { left : View.Config -> Maybe Value -> Maybe (Html Value)
                }
            , browser : View.Config -> Value -> Maybe (Html Value)
            }
        }
    , update : UpdateModel
    , updateByUrl : UpdateByUrlModel
    , init :
        { graph : Value
        }
    }


type alias UpdateModel =
    { model : Update
    , graph :
        { address : Update
        , model : Update
        }
    }


type alias UpdateByUrlModel =
    { graph : UpdateByUrl
    }


type alias Update =
    MsgValue -> StateValue -> ( StateValue, List OutMsg, Cmd Value )


type alias UpdateByUrl =
    String -> StateValue -> ( StateValue, List OutMsg, Cmd Value )


type alias MsgValue =
    Value


type alias StateValue =
    Value


type alias ThingWithPlugins b =
    { b | plugins : Dict String Value }


iterate :
    Dict String Plugin
    -> (String -> svg -> a)
    -> (Plugin -> ThingWithPlugins b -> Value -> svg)
    -> ThingWithPlugins b
    -> List a
iterate plugins mapResult fun thing =
    plugins
        |> Dict.toList
        |> List.filterMap
            (\( pid, plugin ) ->
                Dict.get pid thing.plugins
                    |> Maybe.map (fun plugin thing)
                    |> Maybe.map (mapResult pid)
            )


update :
    String
    -> Dict String Plugin
    -> Dict String StateValue
    -> MsgValue
    -> (UpdateModel -> Update)
    -> ( Dict String StateValue, List OutMsg, List ( String, Cmd Value ) )
update pid plugins states msg fun =
    Maybe.map2
        (\plugin state ->
            let
                ( newState, outMsg, cmd ) =
                    fun plugin.update msg state
            in
            ( Dict.insert pid newState states
            , outMsg
            , [ ( pid, cmd ) ]
            )
        )
        (Dict.get pid plugins)
        (Dict.get pid states)
        |> Maybe.withDefault ( states, [], [] )


updateByUrl :
    String
    -> Plugins
    -> PluginStates
    -> String
    -> (UpdateByUrlModel -> UpdateByUrl)
    -> ( PluginStates, List OutMsg, List ( String, Cmd Value ) )
updateByUrl pid plugins states url fun =
    Maybe.map2
        (\plugin state ->
            let
                ( newState, outMsg, cmd ) =
                    fun plugin.updateByUrl url state
            in
            ( Dict.insert pid newState states
            , outMsg
            , [ ( pid, cmd ) ]
            )
        )
        (Dict.get pid plugins)
        (Dict.get pid states)
        |> Maybe.withDefault ( states, [], [] )


initGraph : Plugins -> PluginStates
initGraph plugins =
    plugins
        |> Dict.map
            (\pid plugin ->
                plugin.init.graph
            )

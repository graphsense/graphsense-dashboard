module Plugin exposing (..)

import Config.View as View
import Dict exposing (Dict)
import Html.Styled as Html exposing (..)
import Json.Encode exposing (Value)
import Model.Graph.Address as Address
import Model.Graph.Id as Id
import Plugin.Model exposing (..)
import Svg.Styled as Svg exposing (..)
import Tuple exposing (..)


type alias Plugins =
    Dict String Plugin


type alias Plugin =
    { view :
        { graph :
            { address :
                { flags : View.Config -> Value -> List (Svg Value)
                , contextMenu : View.Config -> Id.AddressId -> Value -> Maybe Value -> List (Html Value)
                , properties : View.Config -> Value -> List (Html Value)
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
        { graph :
            { model : Value
            , address : Value
            }
        }
    }


type alias UpdateModel =
    { model : Update
    , graph :
        { address : UpdateAddress
        , model : Update
        }
    }


type alias UpdateAddress =
    MsgValue -> StateValue -> StateValue


type alias UpdateByUrlModel =
    { graph : UpdateByUrl
    }


type alias Update =
    MsgValue -> StateValue -> ( StateValue, List (OutMsg Value), Cmd Value )


type alias UpdateByUrl =
    String -> StateValue -> ( StateValue, List (OutMsg Value), Cmd Value )


type alias MsgValue =
    Value


type alias StateValue =
    Value


type alias AddressValue =
    Value


type alias GraphValue =
    Value


type alias ThingWithPlugins b =
    { b | plugins : Dict String Value }


type alias PID =
    String


iterate :
    Dict PID Plugin
    -> (Plugin -> Value -> a)
    -> Dict String Value
    -> List ( PID, a )
iterate plugins fun states =
    plugins
        |> Dict.toList
        |> List.filterMap
            (\( pid, plugin ) ->
                Dict.get pid states
                    |> Maybe.map (fun plugin)
                    |> Maybe.map (pair pid)
            )


update :
    String
    -> Dict String Plugin
    -> Dict String StateValue
    -> MsgValue
    -> (UpdateModel -> Update)
    -> ( Dict String StateValue, List (OutMsg Value), List ( String, Cmd Value ) )
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


updateAddress : String -> Plugins -> Value -> Address.Address -> Address.Address
updateAddress pid plugins msg address =
    Maybe.map2
        (\plugin state ->
            plugin.update.graph.address msg state
        )
        (Dict.get pid plugins)
        (Dict.get pid address.plugins)
        |> Debug.log "Plugin.updateAddress"
        |> Maybe.map
            (\newState ->
                { address
                    | plugins = Dict.insert pid newState address.plugins
                }
            )
        |> Maybe.withDefault address


updateByUrl :
    String
    -> Plugins
    -> PluginStates
    -> String
    -> (UpdateByUrlModel -> UpdateByUrl)
    -> ( PluginStates, List (OutMsg Value), List ( String, Cmd Value ) )
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
                plugin.init.graph.model
            )


initAddress : Plugins -> PluginStates
initAddress plugins =
    plugins
        |> Dict.map
            (\pid plugin ->
                plugin.init.graph.address
            )

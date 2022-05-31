module Plugin exposing (..)

import Config.View as View
import Dict exposing (Dict)
import Html.Styled as Html exposing (..)
import Json.Encode exposing (Value)
import Model.Graph.Address as Address
import Model.Graph.Id as Id
import Plugin.Model exposing (..)
import Regex
import Svg.Styled as Svg exposing (..)
import Tuple exposing (..)
import Url
import Url.Parser exposing ((</>), Parser)


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
            , browser : Config -> View.Config -> Value -> List (Html Value)
            }
        , search :
            { placeholder : View.Config -> List String
            }
        }
    , update : UpdateModel
    , updateByRoute : UpdateByRoute
    , init :
        { graph :
            { model : Value
            , address : Value
            }
        , model : Value
        }
    , effect :
        { search : String -> Cmd Value
        }
    }


type alias UpdateModel =
    { model : Update
    , graph :
        { address : UpdateAddress
        }
    }


type alias UpdateAddress =
    MsgValue -> StateValue -> StateValue


type alias Update =
    Config -> MsgValue -> StateValue -> ( StateValue, List (OutMsg Value), Cmd Value )


type alias UpdateByRoute =
    String -> StateValue -> ( StateValue, List (OutMsg Value), Cmd Value )


type alias RouteValue =
    Value


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
    Config
    -> String
    -> Dict String Plugin
    -> Dict String StateValue
    -> MsgValue
    -> (UpdateModel -> Update)
    -> ( Dict String StateValue, List (OutMsg Value), List ( String, Cmd Value ) )
update pc pid plugins states msg fun =
    Maybe.map2
        (\plugin state ->
            let
                ( newState, outMsg, cmd ) =
                    fun plugin.update pc msg state
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


updateByRoute :
    String
    -> Plugins
    -> PluginStates
    -> String
    -> ( PluginStates, List (OutMsg Value), List ( String, Cmd Value ) )
updateByRoute pid plugins states route =
    Maybe.map2
        (\plugin state ->
            let
                ( newState, outMsg, cmd ) =
                    plugin.updateByRoute route state
            in
            ( Dict.insert pid newState states
            , outMsg
            , [ ( pid, cmd ) ]
            )
        )
        (Dict.get pid plugins)
        (Dict.get pid states)
        |> Maybe.withDefault ( states, [], [] )


init : Plugins -> PluginStates
init plugins =
    plugins
        |> Dict.map
            (\pid plugin ->
                plugin.init.model
            )


initAddress : Plugins -> PluginStates
initAddress plugins =
    plugins
        |> Dict.map
            (\pid plugin ->
                plugin.init.graph.address
            )


parseUrl : Plugins -> String -> Maybe ( String, String )
parseUrl plugins url =
    plugins
        |> Dict.toList
        |> parseUrlHelp url


parseUrlHelp : String -> List ( String, Plugin ) -> Maybe ( String, String )
parseUrlHelp url plugins =
    case plugins of
        [] ->
            Nothing

        ( pid, plugin ) :: rest ->
            let
                regex =
                    "^"
                        ++ pid
                        ++ "[/?#]"
                        |> Regex.fromString
                        |> Maybe.withDefault Regex.never
            in
            if Regex.contains regex url || url == pid then
                let
                    purl =
                        String.dropLeft (String.length pid) url
                in
                Just ( pid, purl )

            else
                parseUrlHelp url rest

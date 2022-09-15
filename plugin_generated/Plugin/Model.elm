module Plugin.Model exposing (..)


type alias ModelState =
    { 
    }


type alias AddressState =
    { 
    }


type alias EntityState =
    { 
    }


type PluginType
    = PluginType


pluginTypeToNamespace : PluginType -> String
pluginTypeToNamespace type_ =
    ""


namespaceToPluginType : String -> Maybe PluginType
namespaceToPluginType str =
    case "" of

        _ ->
            Nothing

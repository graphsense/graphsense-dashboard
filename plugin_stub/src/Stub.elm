module Stub exposing (..)

import PluginInterface
import Stub.Model
import Stub.Msg


type alias Plugin =
    PluginInterface.Plugin Stub.Model.Model Stub.Model.AddressState Stub.Model.EntityState Stub.Msg.Msg Stub.Msg.AddressMsg Stub.Msg.EntityMsg


plugin : String -> Plugin
plugin url =
    { view =
        { addressFlags = Nothing
        , entityFlags = Nothing
        , addressContextMenu = Nothing
        , addressProperties = Nothing
        , entityProperties = Nothing
        , browser = Nothing
        , navbarLeft = Nothing
        , searchPlaceholder = Nothing
        , searchResultList = Nothing
        , sidebar = Nothing
        , main = Nothing
        , hovercards = Nothing
        , title = Nothing
        , profile = Nothing
        }
    , update =
        { update = Nothing
        , updateAddress = Nothing
        , updateEntity = Nothing
        , updateByUrl = Nothing
        , updateGraphByUrl = Nothing
        , addressesAdded = Nothing
        , entitiesAdded = Nothing
        , updateApiKeyHash = Nothing
        , init = ( (), [], Cmd.none )
        , initAddress = Nothing
        , initEntity = Nothing
        , clearSearch = Nothing
        , newGraph = Nothing
        }
    , effects =
        { search = Nothing
        }
    }

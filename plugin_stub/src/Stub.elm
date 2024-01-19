module Stub exposing (..)

import PluginInterface
import PluginInterface.Effects
import PluginInterface.Update
import PluginInterface.View
import RecordSetter exposing (..)
import Stub.Model
import Stub.Msg


type alias Plugin =
    PluginInterface.Plugin Stub.Model.Flags Stub.Model.Model Stub.Model.AddressState Stub.Model.EntityState Stub.Msg.Msg Stub.Msg.AddressMsg Stub.Msg.EntityMsg


plugin : String -> Plugin
plugin url =
    { view =
        PluginInterface.View.init
            |> s_addressFlags Nothing
            |> s_entityFlags Nothing
            |> s_addressContextMenu Nothing
            |> s_addressProperties Nothing
            |> s_entityProperties Nothing
            |> s_browser Nothing
            |> s_graphNavbarLeft Nothing
            |> s_searchPlaceholder Nothing
            |> s_searchResultList Nothing
            |> s_sidebar Nothing
            |> s_navbar Nothing
            |> s_contents Nothing
            |> s_hovercards Nothing
            |> s_title Nothing
            |> s_profile Nothing
    , update =
        PluginInterface.Update.init
            |> s_update Nothing
            |> s_updateAddress Nothing
            |> s_updateEntity Nothing
            |> s_updateByUrl Nothing
            |> s_updateGraphByUrl Nothing
            |> s_addressesAdded Nothing
            |> s_entitiesAdded Nothing
            |> s_updateApiKeyHash Nothing
            |> s_updateApiKey Nothing
            |> s_init (Just (\_ -> ( (), [], Cmd.none )))
            |> s_initAddress Nothing
            |> s_initEntity Nothing
            |> s_clearSearch Nothing
            |> s_newGraph Nothing
            |> s_logout Nothing
    , effects =
        PluginInterface.Effects.init
            |> s_search Nothing
    }

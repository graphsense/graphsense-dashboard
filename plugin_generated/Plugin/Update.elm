module Plugin.Update exposing (..)

import Maybe.Extra
import Model.Graph.Address as Address
import Model.Graph.Entity as Entity
import Model.Graph.Id as Id
import Plugin.Model
import Plugin.Msg
import PluginInterface.Msg
import PluginInterface.Update
import Set exposing (Set)
import Tuple3 as T3


type alias Plugins =
    { 
    }

n m =
  (m, [], Cmd.none)

update : Plugins -> Plugin.Msg.Msg -> Plugin.Model.ModelState -> ( Plugin.Model.ModelState, List Plugin.Msg.OutMsg, Cmd Plugin.Msg.Msg )
update plugins msg state =
    case msg of
        Plugin.Msg.NoOp ->
            ( state, [], Cmd.none ) 


updateAddress : Plugins -> Plugin.Msg.AddressMsg -> Address.Address -> Address.Address
updateAddress plugins msg address =
    address


updateEntity : Plugins -> Plugin.Msg.EntityMsg -> Entity.Entity -> Entity.Entity
updateEntity plugins msg entity =
    entity


addressesAdded : Plugins -> Plugin.Model.ModelState -> Set Id.AddressId -> ( Plugin.Model.ModelState, List Plugin.Msg.OutMsg, Cmd Plugin.Msg.Msg )
addressesAdded plugins state ids =
    n state

entitiesAdded : Plugins -> Plugin.Model.ModelState -> Set Id.EntityId -> ( Plugin.Model.ModelState, List Plugin.Msg.OutMsg, Cmd Plugin.Msg.Msg )
entitiesAdded plugins state ids =
    n state

updateByUrl : Plugin.Model.PluginType -> Plugins -> String -> Plugin.Model.ModelState -> ( Plugin.Model.ModelState, List Plugin.Msg.OutMsg, Cmd Plugin.Msg.Msg )
updateByUrl ns plugins url state =
    n state

updateGraphByUrl : Plugin.Model.PluginType -> Plugins -> String -> Plugin.Model.ModelState -> ( Plugin.Model.ModelState, List Plugin.Msg.OutMsg, Cmd Plugin.Msg.Msg )
updateGraphByUrl ns plugins url state =
    n state

init : Plugins -> Plugin.Model.Flags -> ( Plugin.Model.ModelState, List Plugin.Msg.OutMsg, Cmd Plugin.Msg.Msg )
init plugins flags =
    n {}

initAddress : Plugins -> Plugin.Model.AddressState
initAddress plugins =
    { 
    }


initEntity : Plugins -> Plugin.Model.EntityState
initEntity plugins =
    { 
    }


clearSearch : Plugins -> Plugin.Model.ModelState -> Plugin.Model.ModelState
clearSearch plugins states =
    states

updateApiKeyHash : Plugins -> String -> Plugin.Model.ModelState -> ( Plugin.Model.ModelState, List Plugin.Msg.OutMsg, Cmd Plugin.Msg.Msg )
updateApiKeyHash plugins apiKeyHash state =
    n state

updateApiKey : Plugins -> String -> Plugin.Model.ModelState -> ( Plugin.Model.ModelState, List Plugin.Msg.OutMsg, Cmd Plugin.Msg.Msg )
updateApiKey plugins apiKeyHash state =
    n state

newGraph : Plugins -> Plugin.Model.ModelState -> ( Plugin.Model.ModelState, List Plugin.Msg.OutMsg, Cmd Plugin.Msg.Msg )
newGraph plugins state =
    n state

logout : Plugins -> Plugin.Model.ModelState -> ( Plugin.Model.ModelState, List Plugin.Msg.OutMsg, Cmd Plugin.Msg.Msg )
logout plugins state =
    n state
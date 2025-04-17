module PluginInterface.Update exposing (Return, Update, andThen, init)

import Config.Update as Update
import Model.Graph.Id as Id
import PluginInterface.Msg exposing (InMsg, OutMsg)
import Set exposing (Set)


type alias Return modelState msg addressMsg entityMsg =
    ( modelState, List (OutMsg msg addressMsg entityMsg), Cmd msg )


type alias Update flags modelState addressState entityState msg addressMsg entityMsg =
    { -- update plugin's state
      update : Maybe (Update.Config -> msg -> modelState -> Return modelState msg addressMsg entityMsg)

    -- update by core msg
    , updateByCoreMsg : Maybe (Update.Config -> InMsg -> modelState -> Return modelState msg addressMsg entityMsg)

    -- update an address's plugin state
    , updateAddress : Maybe (addressMsg -> addressState -> addressState)

    -- update an entity's plugin state
    , updateEntity : Maybe (entityMsg -> entityState -> entityState)

    -- update by change of URL
    , updateByUrl : Maybe (Update.Config -> String -> modelState -> Return modelState msg addressMsg entityMsg)

    -- update by change of URL below /graph
    , updateGraphByUrl : Maybe (String -> modelState -> Return modelState msg addressMsg entityMsg)

    -- when entities are added to the graph
    , entitiesAdded : Maybe (Set Id.EntityId -> modelState -> Return modelState msg addressMsg entityMsg)

    -- when user inputs an API key, process the sha256 hash of the key
    , updateApiKeyHash : Maybe (String -> modelState -> Return modelState msg addressMsg entityMsg)

    -- when user inputs an API key
    , updateApiKey : Maybe (String -> modelState -> Return modelState msg addressMsg entityMsg)

    -- initialize plugin's state
    , init : Maybe (flags -> Return modelState msg addressMsg entityMsg)

    -- initialize plugin state on init of address
    , initAddress : Maybe addressState

    -- initialize plugin state on init of entity
    , initEntity : Maybe entityState

    -- when the search results of the search bar need to be cleared
    , clearSearch : Maybe (modelState -> modelState)

    -- when the graph is reset (user clicks "new graph")
    , newGraph : Maybe (modelState -> Return modelState msg addressMsg entityMsg)

    -- when the user logs out
    , logout : Maybe (modelState -> Return modelState msg addressMsg entityMsg)

    -- plugin can decide when core should store a history entry, before a plugin action.
    , shallPushHistory : Maybe (msg -> Bool)
    }


init : Update flags modelState addressState entityState msg addressMsg entityMsg
init =
    { update = Nothing
    , updateByCoreMsg = Nothing
    , updateAddress = Nothing
    , updateEntity = Nothing
    , updateByUrl = Nothing
    , updateGraphByUrl = Nothing
    , entitiesAdded = Nothing
    , updateApiKeyHash = Nothing
    , updateApiKey = Nothing
    , init = Nothing
    , initAddress = Nothing
    , initEntity = Nothing
    , clearSearch = Nothing
    , newGraph = Nothing
    , logout = Nothing
    , shallPushHistory = Nothing
    }


andThen : (modelState -> Return modelState msg addressMsg entityMsg) -> Return modelState msg addressMsg entityMsg -> Return modelState msg addressMsg entityMsg
andThen fun ( modelA, outMsgA, cmdA ) =
    let
        ( modelB, outMsgB, cmdB ) =
            fun modelA
    in
    ( modelB, outMsgA ++ outMsgB, Cmd.batch [ cmdA, cmdB ] )

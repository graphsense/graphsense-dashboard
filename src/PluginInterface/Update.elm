module PluginInterface.Update exposing (..)

import Model.Graph.Id as Id
import PluginInterface.Msg exposing (OutMsg)
import Set exposing (Set)


type alias Update modelState addressState entityState msg addressMsg entityMsg =
    { -- update plugin's state
      update : Maybe (msg -> modelState -> ( modelState, List (OutMsg msg addressMsg entityMsg), Cmd msg ))

    -- update an address's plugin state
    , updateAddress : Maybe (addressMsg -> addressState -> addressState)

    -- update an entity's plugin state
    , updateEntity : Maybe (entityMsg -> entityState -> entityState)

    -- update by change of URL
    , updateByUrl : Maybe (String -> modelState -> ( modelState, List (OutMsg msg addressMsg entityMsg), Cmd msg ))

    -- update by change of URL below /graph
    , updateGraphByUrl : Maybe (String -> modelState -> ( modelState, List (OutMsg msg addressMsg entityMsg), Cmd msg ))

    -- when addresses are added to the graph
    , addressesAdded : Maybe (Set Id.AddressId -> modelState -> ( modelState, List (OutMsg msg addressMsg entityMsg), Cmd msg ))

    -- when entities are added to the graph
    , entitiesAdded : Maybe (Set Id.EntityId -> modelState -> ( modelState, List (OutMsg msg addressMsg entityMsg), Cmd msg ))

    -- when user inputs an API key, process the sha256 hash of the key
    , updateApiKeyHash : Maybe (String -> modelState -> ( modelState, List (OutMsg msg addressMsg entityMsg), Cmd msg ))

    -- initialize plugin's state
    , init : Maybe ( modelState, List (OutMsg msg addressMsg entityMsg), Cmd msg )

    -- initialize plugin state on init of address
    , initAddress : Maybe addressState

    -- initialize plugin state on init of entity
    , initEntity : Maybe entityState

    -- when the search results of the search bar need to be cleared
    , clearSearch : Maybe (modelState -> modelState)

    -- when the graph is reset (user clicks "new graph")
    , newGraph : Maybe (modelState -> ( modelState, List (OutMsg msg addressMsg entityMsg), Cmd msg ))
    }


init : Update modelState addressState entityState msg addressMsg entityMsg
init =
    { update = Nothing
    , updateAddress = Nothing
    , updateEntity = Nothing
    , updateByUrl = Nothing
    , updateGraphByUrl = Nothing
    , addressesAdded = Nothing
    , entitiesAdded = Nothing
    , updateApiKeyHash = Nothing
    , init = Nothing
    , initAddress = Nothing
    , initEntity = Nothing
    , clearSearch = Nothing
    , newGraph = Nothing
    }

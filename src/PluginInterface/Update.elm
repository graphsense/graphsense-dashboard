module PluginInterface.Update exposing (..)

import Model.Graph.Id as Id
import PluginInterface.Msg exposing (OutMsg)
import Set exposing (Set)


type alias Update modelState addressState entityState msg addressMsg entityMsg =
    { update : Maybe (msg -> modelState -> ( modelState, List (OutMsg msg addressMsg entityMsg), Cmd msg ))
    , updateAddress : Maybe (addressMsg -> addressState -> addressState)
    , updateEntity : Maybe (entityMsg -> entityState -> entityState)
    , updateByUrl : Maybe (String -> modelState -> ( modelState, List (OutMsg msg addressMsg entityMsg), Cmd msg ))
    , updateGraphByUrl : Maybe (String -> modelState -> ( modelState, List (OutMsg msg addressMsg entityMsg), Cmd msg ))
    , addressesAdded : Maybe (Set Id.AddressId -> modelState -> ( modelState, List (OutMsg msg addressMsg entityMsg), Cmd msg ))
    , entitiesAdded : Maybe (Set Id.EntityId -> modelState -> ( modelState, List (OutMsg msg addressMsg entityMsg), Cmd msg ))
    , init : Maybe modelState
    , initAddress : Maybe addressState
    , initEntity : Maybe entityState
    , clearSearch : Maybe (modelState -> modelState)
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
    , init = Nothing
    , initAddress = Nothing
    , initEntity = Nothing
    , clearSearch = Nothing
    }

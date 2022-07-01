module Msg.Search exposing (Msg(..))

import Api.Data
import Http
import Json.Encode
import Model.Search exposing (..)
import Plugin.Msg


type Msg
    = BrowserGotSearchResult Api.Data.SearchResult
    | UserInputsSearch String
    | UserHitsEnter
    | UserClicksResult
    | UserClicksResultLine ResultLine
    | RuntimeBounced
    | PluginMsg Plugin.Msg.Msg
    | NoOp

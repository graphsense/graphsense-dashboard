module Msg.Search exposing (Msg(..))

import Api.Data
import Http
import Json.Encode
import Model.Search exposing (..)


type Msg
    = BrowserGotSearchResult Api.Data.SearchResult
    | UserInputsSearch String
    | UserHitsEnter
    | UserClicksResult
    | UserClicksResultLine ResultLine
    | RuntimeBounced
    | PluginMsg String Json.Encode.Value
    | NoOp

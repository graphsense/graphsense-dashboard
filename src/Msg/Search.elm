module Msg.Search exposing (Msg(..))

import Api.Data
import Http
import Json.Encode


type Msg
    = BrowserGotSearchResult Api.Data.SearchResult
    | UserInputsSearch String
    | UserClicksResultLine
    | RuntimeBounced
    | PluginMsg String Json.Encode.Value

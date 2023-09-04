module Msg.Search exposing (Msg(..))

import Api.Data
import Model.Search exposing (..)
import Plugin.Msg


type Msg
    = BrowserGotSearchResult Api.Data.SearchResult
    | UserInputsSearch String
    | UserLeavesSearch
    | UserFocusSearch
    | BouncedBlur
    | UserHitsEnter
    | UserClicksResult
    | UserClicksResultLine ResultLine
    | UserPicksCurrency String
    | RuntimeBounced
    | PluginMsg Plugin.Msg.Msg
    | NoOp

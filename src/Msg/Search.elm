module Msg.Search exposing (Msg(..))

import Api.Data
import Autocomplete
import Model.Search exposing (..)
import Plugin.Msg


type Msg
    = BrowserGotSearchResult String Api.Data.SearchResult
    | UserFocusSearch
    | UserClicksResultLine
    | UserLeavesSearch
    | UserPicksCurrency String
    | UserClickedCloseCurrencyPicker
    | PluginMsg Plugin.Msg.Msg
    | AutocompleteMsg (Autocomplete.Msg ResultLine)
    | NoOp

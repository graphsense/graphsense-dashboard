module Msg.Search exposing (Msg(..))

import Api.Data
import Autocomplete
import Model.Search
import Plugin.Msg


type Msg
    = BrowserGotSearchResult String Api.Data.SearchResult
    | UserFocusSearch
    | UserInputPressed
    | UserClicksResultLine
    | UserClicksRecentResultLine Model.Search.ResultLine
    | UserLeavesSearch
    | UserPicksCurrency String
    | UserClickedCloseCurrencyPicker
    | PluginMsg Plugin.Msg.Msg
    | AutocompleteMsg Autocomplete.Msg
      -- the result of one term of a multi-identifier paste; the String is
      -- that term, so the outcome can be attributed to it
    | BrowserGotMultiSearchResult String Api.Data.SearchResult
      -- the search for one term of a multi-identifier paste failed for good
    | BrowserGotMultiSearchError String
    | NoOp

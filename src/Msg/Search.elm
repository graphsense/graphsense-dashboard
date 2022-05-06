module Msg.Search exposing (Msg(..))

import Api.Data
import Http


type Msg
    = BrowserGotSearchResult Api.Data.SearchResult
    | UserInputsSearch String
    | UserClicksResultLine
    | RuntimeBounced

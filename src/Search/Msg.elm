module Search.Msg exposing (Msg(..))

import Api.Data
import Http


type Msg
    = BrowserGotSearchResult Api.Data.SearchResult
    | UserInputsSearch String
    | UserClicksResultLine
    | RuntimeBounced

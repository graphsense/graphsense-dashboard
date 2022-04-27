module Search.Msg exposing (Msg(..))

import Api.Data
import Http


type Msg
    = BrowserGotSearchResult (Result Http.Error Api.Data.SearchResult)
    | UserInputsSearch String
    | RuntimeBounced

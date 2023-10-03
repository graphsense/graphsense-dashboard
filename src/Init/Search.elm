module Init.Search exposing (..)

import Api.Data
import Autocomplete
import Model.Search exposing (Model, SearchType(..), getLatestBlocks, minSearchInputLength)


init : SearchType -> Model
init searchType =
    { searchType = searchType
    , visible = False
    , autocomplete = Autocomplete.init minSearchInputLength { query = "", choices = [], ignoreList = [] }
    }


initSearchAll : Maybe Api.Data.Stats -> SearchType
initSearchAll stats =
    SearchAll
        { latestBlocks = Maybe.map getLatestBlocks stats |> Maybe.withDefault []
        , pickingCurrency = False
        }

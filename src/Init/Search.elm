module Init.Search exposing (init, initSearchAddressAndTxs, initSearchAll, initWithRecents)

import Api.Data
import Autocomplete
import Model.Search exposing (Model, ResultLine, SearchType(..), getLatestBlocks, minSearchInputLength)


init : SearchType -> Model
init searchType =
    initWithRecents searchType []


initWithRecents : SearchType -> List ResultLine -> Model
initWithRecents searchType recents =
    { searchType = searchType
    , visible = False
    , autocomplete = Autocomplete.init minSearchInputLength { query = "", choices = [], ignoreList = [] }
    , recentSearches = recents
    , userInitiatedFocus = False
    }


initSearchAll : Maybe Api.Data.Stats -> SearchType
initSearchAll stats =
    SearchAll
        { latestBlocks = Maybe.map getLatestBlocks stats |> Maybe.withDefault []
        , pickingCurrency = False
        }


initSearchAddressAndTxs : Maybe (List String) -> SearchType
initSearchAddressAndTxs currencies =
    SearchAddressAndTx
        { currencies_filter = currencies
        }

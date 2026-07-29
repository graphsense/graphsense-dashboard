module Init.Search exposing (init, initSearchAddressAndTxs, initWithRecents)

import Autocomplete
import Model.Search exposing (Model, ResultLine, SearchType(..), minSearchInputLength)


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


initSearchAddressAndTxs : Maybe (List String) -> SearchType
initSearchAddressAndTxs currencies =
    SearchAddressAndTx
        { currencies_filter = currencies
        }

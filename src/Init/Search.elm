module Init.Search exposing (init, initSearchAddressAndTxs, initSearchAll)

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


initSearchAddressAndTxs : Maybe (List String) -> SearchType
initSearchAddressAndTxs currencies =
    SearchAddressAndTx
        { currencies_filter = currencies
        }

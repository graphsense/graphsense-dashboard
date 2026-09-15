module Model.Search exposing (Model, ResultLine(..), SearchType(..), addRecent, addToAutoComplete, filteredRecents, firstResult, getMulti, isMultiIdentifierInput, lastResult, minSearchInputLength, minSearchLengthWithResultExpected, persistRecentSearches, query, searchInputId, selectedValue, setQuery)

import Autocomplete exposing (Autocomplete)
import RecordSetter as Rs
import Util.Data



{- Many Actors have 3 letter names -}


minSearchInputLength : Int
minSearchInputLength =
    2


minSearchLengthWithResultExpected : SearchType -> Int
minSearchLengthWithResultExpected st =
    case st of
        SearchAddressAndTx _ ->
            5

        -- Address and tx search index only starts with 5+ characters
        SearchActorsOnly ->
            minSearchInputLength

        _ ->
            minSearchInputLength


type alias Model =
    { searchType : SearchType
    , visible : Bool
    , autocomplete : Autocomplete ResultLine
    , recentSearches : List ResultLine
    , userInitiatedFocus : Bool
    }


maxRecentSearches : Int
maxRecentSearches =
    10


{-| Dev flip: persist recent searches to localStorage across sessions.
Set to False to keep recents session-only (wiped on reload).
-}
persistRecentSearches : Bool
persistRecentSearches =
    True


isCompatibleWithSearchType : SearchType -> ResultLine -> Bool
isCompatibleWithSearchType searchType rl =
    case ( searchType, rl ) of
        ( SearchAll _, _ ) ->
            True

        ( SearchAddressAndTx _, Address _ _ ) ->
            True

        ( SearchAddressAndTx _, Tx _ _ ) ->
            True

        ( SearchAddressAndTx _, _ ) ->
            False

        ( SearchTagsOnly, Label _ ) ->
            True

        ( SearchTagsOnly, _ ) ->
            False

        ( SearchActorsOnly, Actor _ ) ->
            True

        ( SearchActorsOnly, _ ) ->
            False


filteredRecents : SearchType -> List ResultLine -> List ResultLine
filteredRecents searchType =
    List.filter (isCompatibleWithSearchType searchType)


addRecent : ResultLine -> List ResultLine -> List ResultLine
addRecent rl recents =
    (rl :: List.filter ((/=) rl) recents)
        |> List.take maxRecentSearches


type SearchType
    = SearchAll
        { latestBlocks : List ( String, Int )
        , pickingCurrency : Bool
        }
    | SearchAddressAndTx { currencies_filter : Maybe (List String) }
    | SearchTagsOnly
    | SearchActorsOnly


type ResultLine
    = Address String String
    | Tx String String
    | Block String Int
    | Label String
    | Actor ( String, String )
    | Custom { id : String, label : String }


getMulti : String -> List String
getMulti =
    String.split " "
        >> List.map (String.replace "," "")
        >> List.map String.trim
        >> List.filter (String.isEmpty >> not)


{-| Whether the input is a paste of several identifiers, which enter adds to
the graph one by one (`Update.Pathfinder.multiSearch`). The dropdown hint and
the autocomplete's fetch decision share this test, so such a paste is never
searched as one string -- an empty answer to that would replace the hint with
"no results".
-}
isMultiIdentifierInput : String -> Bool
isMultiIdentifierInput q =
    List.length (Util.Data.parseMultiIdentifierInput q) > 1


query : Model -> String
query { autocomplete } =
    Autocomplete.query autocomplete


setQuery : String -> Model -> Model
setQuery q model =
    { model
        | autocomplete = Autocomplete.setQuery q model.autocomplete
    }


selectedValue : Model -> Maybe ResultLine
selectedValue { autocomplete } =
    Autocomplete.selectedValue autocomplete


firstResult : Model -> Maybe ResultLine
firstResult { autocomplete } =
    Autocomplete.choices autocomplete |> List.head


lastResult : Model -> Maybe ResultLine
lastResult { autocomplete } =
    Autocomplete.choices autocomplete |> List.reverse |> List.head


addToAutoComplete : ResultLine -> Model -> Model
addToAutoComplete rl m =
    let
        c =
            Autocomplete.choices m.autocomplete

        nc =
            c ++ [ rl ]
    in
    m |> Rs.s_autocomplete (m.autocomplete |> Autocomplete.setChoices nc)


searchInputId : String
searchInputId =
    "search_input"

module Model.Search exposing (Model, ResultLine(..), SearchType(..), addToAutoComplete, firstResult, getLatestBlocks, getMulti, isLikelyPathSearchInput, lastResult, minSearchInputLength, query, selectedValue, setIsPickingCurrency, setQuery)

import Api.Data
import Autocomplete exposing (Autocomplete)
import RecordSetter as Rs



{- Many Actors have 3 letter names -}


minSearchInputLength : Int
minSearchInputLength =
    2


type alias Model =
    { searchType : SearchType
    , visible : Bool
    , autocomplete : Autocomplete ResultLine
    }


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


isLikelyPathSearchInput : String -> Bool
isLikelyPathSearchInput q =
    let
        mul =
            getMulti q
    in
    List.length mul
        > 1
        && List.all (\i -> String.length i > 20) mul


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


getLatestBlocks : Api.Data.Stats -> List ( String, Int )
getLatestBlocks =
    .currencies
        >> List.map (\{ name, noBlocks } -> ( name, noBlocks - 1 ))


setIsPickingCurrency : Model -> Model
setIsPickingCurrency model =
    { model
        | searchType =
            case model.searchType of
                SearchAll sa ->
                    { sa | pickingCurrency = True }
                        |> SearchAll

                SearchAddressAndTx x ->
                    SearchAddressAndTx x

                SearchTagsOnly ->
                    SearchTagsOnly

                SearchActorsOnly ->
                    SearchActorsOnly
    }

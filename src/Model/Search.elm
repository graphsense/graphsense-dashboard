module Model.Search exposing (..)

import Api.Data
import Bounce exposing (Bounce)



{- Many Actors have 3 letter names -}


minSearchInputLength : Int
minSearchInputLength =
    2


type alias Model =
    { loading : Bool
    , visible : Bool
    , found : Maybe Api.Data.SearchResult
    , input : String
    , bounce : Bounce
    }


type ResultLine
    = Address String
    | Tx String
    | Block Int
    | Label String
    | Actor ( String, String )


getMulti : Model -> List String
getMulti model =
    String.split " " model.input
        |> List.map (String.replace "," "")
        |> List.map String.trim
        |> List.filter (String.isEmpty >> not)


isLikelyPathSearchInput : Model -> Bool
isLikelyPathSearchInput model =
    let
        mul =
            getMulti model
    in
    List.length mul
        > 1
        && List.all (\i -> String.length i > 20) mul

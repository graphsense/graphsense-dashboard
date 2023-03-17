module Model.Search exposing (..)

import Api.Data
import Bounce exposing (Bounce)
import RemoteData exposing (WebData)


minSearchInputLength : Int
minSearchInputLength =
    4


type alias Model =
    { loading : Bool
    , visible : Bool
    , found : Maybe Api.Data.SearchResult
    , input : String
    , batch : Maybe ( String, List String )
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

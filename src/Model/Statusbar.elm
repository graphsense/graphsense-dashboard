module Model.Statusbar exposing (Model, getMessage, loadingActorKey, loadingActorTagsKey, loadingAddressEntityKey, loadingAddressKey, searchNeighborsKey)

import Dict exposing (Dict)
import Http


searchNeighborsKey : String
searchNeighborsKey =
    "{6}: searching {0} of {1} with {2} (depth: {3}, breadth: {4}, skip if more than {5} addresses)"


loadingAddressKey : String
loadingAddressKey =
    "{1}: loading address {0}"


loadingActorKey : String
loadingActorKey =
    "Loading Actor {0}"


loadingActorTagsKey : String
loadingActorTagsKey =
    "Loading Tags of Actor {0}"


loadingAddressEntityKey : String
loadingAddressEntityKey =
    "{1}: loading entity for address {0}"


type alias Model =
    { messages : Dict String ( String, List String )
    , log : List ( String, List String, Maybe Http.Error )
    , visible : Bool
    , lastBlocks : List ( String, Int )
    }


getMessage : String -> Model -> Maybe ( String, List String )
getMessage key { messages } =
    Dict.get key messages

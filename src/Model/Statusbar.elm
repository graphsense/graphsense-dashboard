module Model.Statusbar exposing (Model, getMessage, loadingActorKey, loadingActorTagsKey, loadingAddressEntityKey, loadingAddressKey, searchNeighborsKey)

import Dict exposing (Dict)
import Http


searchNeighborsKey : String
searchNeighborsKey =
    "Statusbar-search-parameters"


loadingAddressKey : String
loadingAddressKey =
    "Statusbar-loading-address"


loadingActorKey : String
loadingActorKey =
    "Statusbar-loading-actor"


loadingActorTagsKey : String
loadingActorTagsKey =
    "Statusbar-loading-tags-of-actor"


loadingAddressEntityKey : String
loadingAddressEntityKey =
    "Statusbar-loading-entity-for-address"


type alias Model =
    { messages : Dict String ( String, List String )
    , log : List ( String, List String, Maybe Http.Error )
    , visible : Bool
    , lastBlocks : List ( String, Int )
    }


getMessage : String -> Model -> Maybe ( String, List String )
getMessage key { messages } =
    Dict.get key messages

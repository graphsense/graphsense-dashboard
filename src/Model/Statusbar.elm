module Model.Statusbar exposing (..)

import Config.View as View
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Http
import Model.Graph.Id as Id
import Model.Graph.Search as Search


searchNeighborsKey : String
searchNeighborsKey =
    "{6}: searching {0} of {1} with {2} (depth: {3}, breadth: {4}, skip if more than {5} addresses)"


loadingAddressKey : String
loadingAddressKey =
    "{1}: loading address {0}"


loadingAddressEntityKey : String
loadingAddressEntityKey =
    "{1}: loading entity for address {0}"


type alias Model =
    { messages : Dict String ( String, List String )
    , log : List ( String, List String, Maybe Http.Error )
    , visible : Bool
    , lastBlocks : List ( String, Int )
    }

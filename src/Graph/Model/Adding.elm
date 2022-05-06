module Graph.Model.Adding exposing (..)

import Set exposing (Set)


type alias Model =
    { addresses : Set ( String, String )
    , entities : Set ( Int, String )
    , labels : Set String
    }

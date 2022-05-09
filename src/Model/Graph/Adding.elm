module Model.Graph.Adding exposing (..)

import Set exposing (Set)


type alias Model =
    { addresses : Set ( String, String )
    , entities : Set ( String, Int )
    , labels : Set String
    }

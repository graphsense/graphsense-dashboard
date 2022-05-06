module Graph.Model.Tool exposing (..)

import Graph.Msg exposing (Msg)
import Html exposing (Html)


type alias Tool =
    { icon : Html Msg
    , title : String
    , msg : Msg
    }

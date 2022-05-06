module Model.Graph.Tool exposing (..)

import Html exposing (Html)
import Msg.Graph exposing (Msg)


type alias Tool =
    { icon : Html Msg
    , title : String
    , msg : Msg
    }

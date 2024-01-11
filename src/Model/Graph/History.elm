module Model.Graph.History exposing (..)

import Color exposing (Color)
import IntDict exposing (IntDict)
import Model.Graph.Layer exposing (Layer)


type alias Model =
    { past : List (IntDict Layer)
    , future : List (IntDict Layer)
    }


type alias Entry =
    { layers : IntDict Layer
    , highlights : List ( String, Color )
    }


hasPast : Model -> Bool
hasPast { past } =
    List.isEmpty past |> not


hasFuture : Model -> Bool
hasFuture { future } =
    List.isEmpty future |> not

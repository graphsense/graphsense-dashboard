module Model.Graph.History exposing (..)

import Color exposing (Color)
import IntDict exposing (IntDict)
import List.Extra
import Model.Graph.Layer exposing (Layer)


type alias Model =
    { past : List Entry
    , future : List Entry
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


unconsPast : Model -> Maybe ( Entry, List Entry )
unconsPast { past } =
    List.Extra.uncons past


unconsFuture : Model -> Maybe ( Entry, List Entry )
unconsFuture { future } =
    List.Extra.uncons future

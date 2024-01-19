module Model.Graph.History exposing (..)

import Color exposing (Color)
import Init.Graph.History.Entry as Entry
import IntDict exposing (IntDict)
import List.Extra
import Model.Graph.History.Entry as Entry
import Model.Graph.Layer exposing (Layer)


type alias Model =
    { past : List Entry.Model
    , future : List Entry.Model
    }


hasPast : Model -> Entry.Model -> Bool
hasPast { past } entry =
    (List.isEmpty past |> not)
        && ((past /= [ Entry.init ])
                || (entry /= Entry.init)
           )


hasFuture : Model -> Bool
hasFuture { future } =
    List.isEmpty future |> not


unconsPast : Model -> Maybe ( Entry.Model, List Entry.Model )
unconsPast { past } =
    List.Extra.uncons past


unconsFuture : Model -> Maybe ( Entry.Model, List Entry.Model )
unconsFuture { future } =
    List.Extra.uncons future

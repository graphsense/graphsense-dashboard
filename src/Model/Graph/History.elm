module Model.Graph.History exposing (..)

import List.Extra


type alias Model entry =
    { past : List entry
    , future : List entry
    }


hasPast : entry -> Model entry -> entry -> Bool
hasPast init { past } entry =
    (List.isEmpty past |> not)
        && ((past /= [ init ])
                || (entry /= init)
           )


hasFuture : Model entry -> Bool
hasFuture { future } =
    List.isEmpty future |> not


unconsPast : Model entry -> Maybe ( entry, List entry )
unconsPast { past } =
    List.Extra.uncons past


unconsFuture : Model entry -> Maybe ( entry, List entry )
unconsFuture { future } =
    List.Extra.uncons future

module Model.Graph.History exposing (Model)


type alias Model entry =
    { past : List entry
    , future : List entry
    }

module Model.Graph.Link exposing (..)

import Api.Data


type alias Link a =
    { node : a
    , labels : Maybe (List String)
    , noTxs : Int
    , value : Api.Data.Values
    }

module Model.Graph.Link exposing (..)

import Api.Data
import Dict exposing (Dict)


type alias Link a =
    { node : a
    , forceShow : Bool
    , link : LinkData
    , selected : Bool
    }


type LinkData
    = LinkData
        { labels : Maybe (List String)
        , noTxs : Int
        , value : Api.Data.Values
        , tokenValues : Maybe (Dict String Api.Data.Values)
        }
    | PlaceholderLinkData


fromNeighbor : { a | labels : Maybe (List String), noTxs : Int, value : Api.Data.Values, tokenValues : Maybe (Dict String Api.Data.Values) } -> LinkData
fromNeighbor { labels, noTxs, value, tokenValues } =
    LinkData
        { labels = labels
        , noTxs = noTxs
        , value = value
        , tokenValues = tokenValues
        }

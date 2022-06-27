module Model.Graph.Link exposing (..)

import Api.Data


type alias Link a =
    { node : a
    , link : LinkData
    }


type LinkData
    = LinkData
        { labels : Maybe (List String)
        , noTxs : Int
        , value : Api.Data.Values
        }
    | PlaceholderLinkData


fromNeighbor : { a | labels : Maybe (List String), noTxs : Int, value : Api.Data.Values } -> LinkData
fromNeighbor { labels, noTxs, value } =
    LinkData
        { labels = labels
        , noTxs = noTxs
        , value = value
        }

module Model.Graph.Link exposing (..)

import Api.Data


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
        }
    | PlaceholderLinkData


fromNeighbor : { a | labels : Maybe (List String), noTxs : Int, value : Api.Data.Values } -> LinkData
fromNeighbor { labels, noTxs, value } =
    LinkData
        { labels = labels
        , noTxs = noTxs
        , value = value
        }

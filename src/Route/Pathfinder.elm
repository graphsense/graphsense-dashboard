module Route.Pathfinder exposing (..)

import Util.Url.Parser as P exposing (..)


type Route
    = Root


toUrl : Route -> String
toUrl _ =
    "/"


parser : Parser (Route -> a) a
parser =
    map Root P.top

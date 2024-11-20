module Util.Url.Parser.Internal exposing (QueryParser(..))

import Dict exposing (Dict)


type QueryParser a
    = Parser (Dict String (List String) -> a)

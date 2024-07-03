module Types exposing (..)

import Api.Raw exposing (Rectangle)
import Dict exposing (Dict)
import Elm exposing (Expression)


type alias Config =
    { propertyExpressions : ComponentPropertyExpressions
    , attributes : Expression
    }


type alias Metadata =
    { name : String
    , bbox : Rectangle
    , strokeWidth : Float
    }


type alias OriginAdjust =
    { x : Float
    , y : Float
    }


type alias ComponentPropertyExpressions =
    Dict String Expression

module Types exposing (..)

import Api.Raw exposing (Rectangle)
import Dict exposing (Dict)
import Elm exposing (Expression)


type alias Config =
    { propertyExpressions : Dict String ComponentPropertyExpressions
    , attributes : Expression
    , instances : Expression
    }


type alias Details =
    { name : String
    , bbox : Rectangle
    , strokeWidth : Float
    , styles : List Elm.Expression
    }


type alias OriginAdjust =
    { x : Float
    , y : Float
    }


type alias ComponentPropertyExpressions =
    Dict String Expression

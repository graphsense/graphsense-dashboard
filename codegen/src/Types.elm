module Types exposing (..)

import Api.Raw exposing (Rectangle, Size)
import Dict exposing (Dict)
import Elm exposing (Expression)


type alias Config =
    { propertyExpressions : Dict String ComponentPropertyExpressions
    , positionRelatively : Maybe OriginAdjust
    , attributes : Expression
    , instances : Expression
    , children : Expression
    , colorMap : ColorMap
    , parentName : String
    , componentName : String
    , instanceName : String
    , showId : Bool
    }


type alias Details =
    { name : String

    -- enclosing instance name
    , instanceName : String
    , bbox : Rectangle
    , renderedSize : Size
    , strokeWidth : Float
    , styles : List Elm.Expression
    }


type alias OriginAdjust =
    { x : Float
    , y : Float
    }


type alias ComponentPropertyExpressions =
    Dict String Expression


type alias ColorMap =
    Dict String String

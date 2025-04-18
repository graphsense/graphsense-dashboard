module Types exposing (..)

import Api.Raw exposing (ComponentPropertiesTrait, FrameTraits, Rectangle, Size)
import Dict exposing (Dict)
import Elm exposing (Expression)
import Elm.Annotation exposing (Annotation)


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
    }


type alias Styles =
    List Elm.Expression


type alias OriginAdjust =
    { x : Float
    , y : Float
    }


type alias ComponentPropertyExpressions =
    Dict String Expression


type alias ColorMap =
    Dict String String


type alias ComponentNodeOrSet =
    { componentPropertiesTrait : ComponentPropertiesTrait, frameTraits : FrameTraits }


type alias FormatSpecifics =
    { toStyles : ColorMap -> FrameTraits -> ( Styles, List Styles )
    , withFrameTraitsNodeToExpression : Config -> String -> String -> FrameTraits -> Elm.Expression
    , elementAnnotation : Annotation -> Annotation
    , attributeAnnotation : Annotation -> Annotation
    }

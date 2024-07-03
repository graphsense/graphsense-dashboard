module Generate.Svg.DefaultShapeTraits exposing (..)

import Api.Raw exposing (..)
import Elm
import Generate.Svg.HasGeometryTrait as HasGeometryTrait
import Generate.Util exposing (..)


toCss : DefaultShapeTraits -> List Elm.Expression
toCss node =
    HasGeometryTrait.toCss node.hasGeometryTrait

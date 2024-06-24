module Generate.Html.DefaultShapeTraits exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Util exposing (..)
import Generate.Html.HasGeometryTrait as HasGeometryTrait


toCss : DefaultShapeTraits -> List Elm.Expression
toCss node =
    HasGeometryTrait.toCss node.hasGeometryTrait

module Generate.Html.DefaultShapeTraits exposing (..)

import Api.Raw exposing (..)
import Elm
import Generate.Common.DefaultShapeTraits as Common
import Generate.Html.HasGeometryTrait as HasGeometryTrait
import Generate.Util exposing (..)
import Types exposing (Details)


toStyles : DefaultShapeTraits -> List Elm.Expression
toStyles node =
    HasGeometryTrait.toStyles node.hasGeometryTrait


toDetails : { a | defaultShapeTraits : DefaultShapeTraits } -> Details
toDetails node =
    Common.toDetails (toStyles node.defaultShapeTraits) node

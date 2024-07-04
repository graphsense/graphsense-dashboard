module Generate.Html.HasGeometryTrait exposing (..)

import Api.Raw exposing (..)
import Elm
import Generate.Html.MinimalFillsTrait as MinimalFillsTrait
import Gen.Css as Css
import Generate.Util exposing (..)


toStyles : HasGeometryTrait -> List Elm.Expression
toStyles node =
    MinimalFillsTrait.toStyles node.minimalFillsTrait

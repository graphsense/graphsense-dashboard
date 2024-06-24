module Generate.Html.HasGeometryTrait exposing (..)

import Api.Raw exposing (..)
import Elm
import Generate.Html.MinimalFillsTrait as MinimalFillsTrait
import Gen.Css as Css
import Generate.Util exposing (..)


toCss : HasGeometryTrait -> List Elm.Expression
toCss node =
    MinimalFillsTrait.toCss node.minimalFillsTrait

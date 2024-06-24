module Generate.Html.MinimalFillsTrait exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Util.Paint as Paint
import Generate.Util exposing (..)


toCss : MinimalFillsTrait -> List Elm.Expression
toCss node =
    []
        |> a (Paint.toCss >> Maybe.map Css.color) (Just node.fills)

module Generate.Html.MinimalFillsTrait exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Util.Paint as Paint
import Generate.Util exposing (..)


toStyles : MinimalFillsTrait -> List Elm.Expression
toStyles node =
    []
        |> a (Paint.toStyles >> Maybe.map Css.color) (Just node.fills)

module Generate.Html.MinimalFillsTrait exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Util exposing (..)
import Generate.Util.Paint as Paint
import Types exposing (ColorMap)


toStyles : ColorMap -> MinimalFillsTrait -> List Elm.Expression
toStyles colorMap node =
    []
        |> a (Paint.toStylesString colorMap >> Maybe.map (Css.property "color")) (Just node.fills)

module Generate.Svg.MinimalFillsTrait exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Util exposing (..)
import Generate.Util.Paint as Paint
import Types exposing (ColorMap)


toStyles : ColorMap -> MinimalFillsTrait -> List Elm.Expression
toStyles colorMap node =
    []
        |> m (Paint.toStylesString colorMap >> Maybe.withDefault "transparent" >> Css.property "fill") (Just node.fills)
        |> a opacity (Just node.fills)


opacity : List Paint -> Maybe Elm.Expression
opacity =
    Paint.getBasePaint
        >> Maybe.andThen .opacity
        >> Maybe.map String.fromFloat
        >> Maybe.map (Css.property "opacity")

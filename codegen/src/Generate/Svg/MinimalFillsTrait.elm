module Generate.Svg.MinimalFillsTrait exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Util exposing (..)
import Generate.Util.Paint as Paint


toCss : MinimalFillsTrait -> List Elm.Expression
toCss node =
    []
        |> m (Paint.toCss >> Maybe.withDefault Css.transparent >> Css.fill) (Just node.fills)
        |> a
            (Paint.getBasePaint
                >> Maybe.andThen .opacity
                >> Maybe.map String.fromFloat
                >> Maybe.map (Css.property "opacity")
            )
            (Just node.fills)

module Generate.Util.RGBA exposing (..)

import Api.Raw exposing (RGBA)
import Dict as Dict
import Elm
import Gen.Css as Css
import Round as R
import String.Format as Format
import Types exposing (ColorMap)


toStyles : ColorMap -> RGBA -> Elm.Expression
toStyles colorMap ({ r, g, b, a } as c) =
    Dict.get (toStylesString Dict.empty c) colorMap
        |> Maybe.map (toVarString >> Elm.string)
        |> Maybe.withDefault
            (let
                f =
                    round << (*) 255
             in
             Css.rgba
                (f r)
                (f g)
                (f b)
                (R.roundNum 2 a)
            )


toVarString : String -> String
toVarString s =
    "var(--" ++ s ++ ")"


toStylesString : ColorMap -> RGBA -> String
toStylesString colorMap { r, g, b, a } =
    let
        f =
            String.fromInt << round << (*) 255

        str =
            "rgba({{ r }}, {{ g }}, {{ b }}, {{ a }})"
                |> Format.namedValue "r" (f r)
                |> Format.namedValue "g" (f g)
                |> Format.namedValue "b" (f b)
                |> Format.namedValue "a" (String.fromFloat (R.roundNum 2 a))
    in
    Dict.get str colorMap
        |> Maybe.map toVarString
        |> Maybe.withDefault str

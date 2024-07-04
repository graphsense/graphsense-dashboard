module Generate.Util.RGBA exposing (..)

import Api.Raw exposing (RGBA)
import Elm
import Gen.Css as Css
import String.Format as Format


toStyles : RGBA -> Elm.Expression
toStyles { r, g, b, a } =
    let
        f =
            round << (*) 255
    in
    Css.rgba
        (f r)
        (f g)
        (f b)
        a


toStylesString : RGBA -> Elm.Expression
toStylesString { r, g, b, a } =
    let
        f =
            String.fromInt << round << (*) 255
    in
    "rgba({{ r }}, {{ g }}, {{ b }}, {{ a }})"
        |> Format.namedValue "r" (f r)
        |> Format.namedValue "g" (f g)
        |> Format.namedValue "b" (f b)
        |> Format.namedValue "a" (String.fromFloat a)
        |> Elm.string



module Generate.Svg.CornerTrait exposing (..)

import Api.Raw exposing (CornerTrait)
import Elm
import Gen.Css as Css
import Gen.Svg.Styled.Attributes as Svg
import Generate.Util exposing (..)


toAttributes : CornerTrait -> List Elm.Expression
toAttributes node =
    []
        |> m cornerRadius node.cornerRadius


cornerRadius : Float -> Elm.Expression
cornerRadius =
    String.fromFloat >> Svg.rx


rectangleCornerRadii : List Float -> List Elm.Expression
rectangleCornerRadii radii =
    -- in case we need that we have to use the path geometry of rectangle
    -- there is no way of setting individual corner radius on a rect
    []

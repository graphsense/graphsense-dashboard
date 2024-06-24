module Generate.Html.TextNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Html.Styled
import Gen.Html.Styled.Attributes
import Generate.Html.DefaultShapeTraits as DefaultShapeTraits
import Generate.Html.TypeStyle as TypeStyle
import Tuple exposing (pair)


toExpressions : TextNode -> List ( String, Elm.Expression )
toExpressions node =
    Gen.Html.Styled.div
        [ toCss node
            |> Gen.Html.Styled.Attributes.css
        ]
        [ Gen.Html.Styled.text node.characters
        ]
        |> pair node.defaultShapeTraits.isLayerTrait.name
        |> List.singleton


toCss : TextNode -> List Elm.Expression
toCss node =
    TypeStyle.toCss node.style
        ++ DefaultShapeTraits.toCss node.defaultShapeTraits

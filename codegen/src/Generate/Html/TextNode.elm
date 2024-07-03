module Generate.Html.TextNode exposing (..)

import Api.Raw exposing (..)
import Dict
import Elm
import Elm.Op
import Gen.Html.Styled
import Gen.Html.Styled.Attributes as Attributes
import Generate.Common.TextNode exposing (getName)
import Generate.Html.DefaultShapeTraits as DefaultShapeTraits
import Generate.Html.TypeStyle as TypeStyle
import Generate.Util exposing (getElementAttributes)
import Types exposing (Config)


toExpressions : Config -> TextNode -> List Elm.Expression
toExpressions config node =
    Gen.Html.Styled.call_.div
        (getName node
            |> getElementAttributes config
            |> Elm.Op.append
                ([ toCss node |> Attributes.css ]
                    |> Elm.list
                )
        )
        (node.defaultShapeTraits.isLayerTrait.componentPropertyReferences
            |> Maybe.andThen (Dict.get "characters")
            |> Maybe.andThen (\ref -> Dict.get ref config.propertyExpressions)
            |> Maybe.map Gen.Html.Styled.call_.text
            |> Maybe.withDefault (Gen.Html.Styled.text node.characters)
            |> List.singleton
            |> Elm.list
        )
        |> List.singleton


toCss : TextNode -> List Elm.Expression
toCss node =
    TypeStyle.toCss node.style
        ++ DefaultShapeTraits.toCss node.defaultShapeTraits

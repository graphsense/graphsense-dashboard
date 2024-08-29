module Generate.Html.TextNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Elm.Op
import Gen.Html.Styled
import Gen.Html.Styled.Attributes as Attributes
import Gen.Maybe
import Generate.Common.DefaultShapeTraits as Common
import Generate.Html.DefaultShapeTraits as DefaultShapeTraits
import Generate.Html.MinimalFillsTrait as MinimalFillsTrait
import Generate.Html.TypeStyle as TypeStyle
import Generate.Util exposing (getElementAttributes, getTextProperty, withVisibility)
import Types exposing (Config, Details)


toExpressions : Config -> String -> TextNode -> List Elm.Expression
toExpressions config componentName node =
    Elm.get (Common.getName node) config.instances
        |> Gen.Maybe.withDefault
            (Gen.Html.Styled.call_.div
                (Common.getName node
                    |> getElementAttributes config
                    |> Elm.Op.append
                        ([ toStyles node |> Attributes.css ]
                            |> Elm.list
                        )
                )
                (getTextProperty componentName config.propertyExpressions node.defaultShapeTraits.isLayerTrait.componentPropertyReferences
                    |> Maybe.map Gen.Html.Styled.call_.text
                    |> Maybe.withDefault (Gen.Html.Styled.text node.characters)
                    |> withVisibility componentName config.propertyExpressions node.defaultShapeTraits.isLayerTrait.componentPropertyReferences
                    |> List.singleton
                    |> Elm.list
                )
            )
        |> List.singleton


toStyles : TextNode -> List Elm.Expression
toStyles node =
    TypeStyle.toStyles node.style
        ++ MinimalFillsTrait.toStyles node.defaultShapeTraits.hasGeometryTrait.minimalFillsTrait
        ++ DefaultShapeTraits.toStyles node.defaultShapeTraits


toDetails : TextNode -> Details
toDetails node =
    Common.toDetails (toStyles node) node

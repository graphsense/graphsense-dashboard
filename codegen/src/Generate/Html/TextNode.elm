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
import Generate.Util exposing (addIdAttribute, callStyles, getElementAttributes, getTextProperty, withVisibility)
import Types exposing (ColorMap, Config, Details)


toExpressions : Config -> String -> TextNode -> List Elm.Expression
toExpressions config componentName node =
    let
        name =
            Common.getName node.defaultShapeTraits
    in
    if Common.isHidden node then
        []

    else
        Elm.get name config.instances
            |> Gen.Maybe.withDefault
                (Gen.Html.Styled.call_.div
                    (name
                        |> getElementAttributes config
                        |> Elm.Op.append
                            (callStyles config name
                                |> Attributes.call_.css
                                |> List.singleton
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


toStyles : ColorMap -> TextNode -> List Elm.Expression
toStyles colorMap node =
    TypeStyle.toStyles colorMap node.style
        ++ MinimalFillsTrait.toStyles colorMap node.defaultShapeTraits.hasGeometryTrait.minimalFillsTrait
        ++ DefaultShapeTraits.toStyles colorMap node.defaultShapeTraits

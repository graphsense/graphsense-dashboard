module Generate.Html.TextNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Elm.Op
import Gen.Css as Css
import Gen.Html.Styled
import Gen.Html.Styled.Attributes as Attributes
import Gen.Maybe
import Generate.Common.DefaultShapeTraits as Common
import Generate.Html.DefaultShapeTraits as DefaultShapeTraits
import Generate.Html.MinimalFillsTrait as MinimalFillsTrait
import Generate.Html.TypeStyle as TypeStyle
import Generate.Util exposing (getElementAttributes, getTextProperty, withVisibility)
import Types exposing (ColorMap, Config, Details)


toExpressions : Config -> String -> TextNode -> List Elm.Expression
toExpressions config componentName node =
    if Common.isHidden node then
        []

    else
        Elm.get (Common.getName node) config.instances
            |> Gen.Maybe.withDefault
                (Gen.Html.Styled.call_.div
                    (Common.getName node
                        |> getElementAttributes config
                        |> Elm.Op.append
                            ([ toStyles config.colorMap node |> Attributes.css ]
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
    Css.whiteSpace Css.noWrap
        :: TypeStyle.toStyles colorMap node.style
        ++ MinimalFillsTrait.toStyles colorMap node.defaultShapeTraits.hasGeometryTrait.minimalFillsTrait
        ++ DefaultShapeTraits.toStyles colorMap node.defaultShapeTraits


toDetails : ColorMap -> TextNode -> Details
toDetails colorMap node =
    Common.toDetails (toStyles colorMap node) node

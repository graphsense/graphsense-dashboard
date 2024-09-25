module Generate.Svg.DefaultShapeTraits exposing (..)

import Api.Raw exposing (..)
import Elm
import Elm.Op
import Gen.Css as Css
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes as Attributes
import Generate.Common.DefaultShapeTraits as Common
import Generate.Svg.HasGeometryTrait as HasGeometryTrait
import Generate.Svg.MinimalFillsTrait as MinimalFillsTrait
import Generate.Util exposing (..)
import Generate.Util.Paint as Paint
import List.Nonempty as NList
import Types exposing (Config, Details)


toExpressions : Config -> String -> { a | defaultShapeTraits : DefaultShapeTraits } -> List Elm.Expression
toExpressions config componentName node =
    if Common.isHidden node then
        []

    else
        Gen.Svg.Styled.g
            (toAttributes node)
            [ toStrokePaths config node
            , toFillPaths config node
            ]
            |> withVisibility componentName config.propertyExpressions node.defaultShapeTraits.isLayerTrait.componentPropertyReferences
            |> List.singleton


renderPath : { a | path : String } -> Elm.Expression
renderPath { path } =
    Gen.Svg.Styled.path
        [ Attributes.d path
        ]
        []


toStrokePaths : Config -> { a | defaultShapeTraits : DefaultShapeTraits } -> Elm.Expression
toStrokePaths config node =
    let
        strokes =
            node.defaultShapeTraits.hasGeometryTrait.strokes

        css =
            []
                |> a
                    (Paint.toStylesString
                        >> Maybe.map (Css.call_.property (Elm.string "fill"))
                    )
                    strokes
                |> a
                    (Paint.getBasePaint
                        >> Maybe.andThen .opacity
                        >> Maybe.map String.fromFloat
                        >> Maybe.map (Css.property "opacity")
                    )
                    strokes
    in
    node.defaultShapeTraits.strokeGeometry
        |> Maybe.andThen NList.fromList
        |> Maybe.map NList.toList
        |> Maybe.map
            (List.map renderPath
                >> Elm.list
                >> Gen.Svg.Styled.call_.g
                    (Common.getName node
                        |> getElementAttributes config
                        |> Elm.Op.append
                            ([ css |> Attributes.css ]
                                |> Elm.list
                            )
                    )
            )
        |> Maybe.withDefault (Gen.Svg.Styled.g [] [])


toFillPaths : Config -> { a | defaultShapeTraits : DefaultShapeTraits } -> Elm.Expression
toFillPaths config node =
    let
        css =
            node.defaultShapeTraits.hasGeometryTrait.minimalFillsTrait
                |> MinimalFillsTrait.toStyles
    in
    node.defaultShapeTraits.fillGeometry
        |> Maybe.map (List.map renderPath)
        |> Maybe.withDefault []
        |> Elm.list
        |> Gen.Svg.Styled.call_.g
            (Common.getName node
                |> getElementAttributes config
                |> Elm.Op.append
                    ([ css |> Attributes.css ]
                        |> Elm.list
                    )
            )


toStyles : DefaultShapeTraits -> List Elm.Expression
toStyles node =
    HasGeometryTrait.toStyles node.hasGeometryTrait


toDetails : { a | defaultShapeTraits : DefaultShapeTraits } -> Details
toDetails node =
    Common.toDetails (toStyles node.defaultShapeTraits) node


toAttributes : { a | defaultShapeTraits : DefaultShapeTraits } -> List Elm.Expression
toAttributes node =
    [ toTranslate node.defaultShapeTraits.absoluteBoundingBox
        |> Attributes.transform
    ]

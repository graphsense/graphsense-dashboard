module Generate.Svg.TextNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Elm.Op
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes as Attributes
import Generate.Common.TextNode exposing (getName)
import Generate.Svg.HasGeometryTrait as HasGeometryTrait
import Generate.Svg.TypeStyle as TypeStyle
import Generate.Util exposing (callStyles, getElementAttributes, getTextProperty, m, mm, withVisibility)
import Types exposing (ColorMap, Config, Details)
import Generate.Svg.HasBlendModeAndOpacityTrait as HasBlendModeAndOpacityTrait


toExpressions : Config -> String -> TextNode -> List Elm.Expression
toExpressions config componentName node =
    let
        name =
            getName node
    in
    Gen.Svg.Styled.call_.text_
        (name
            |> getElementAttributes config
            |> Elm.Op.append
                ((callStyles config name |> Attributes.call_.css)
                    :: toAttributes node
                    |> Elm.list
                )
        )
        (getTextProperty componentName config.propertyExpressions node.defaultShapeTraits.isLayerTrait.componentPropertyReferences
            |> Maybe.map Gen.Svg.Styled.call_.text
            |> Maybe.withDefault (Gen.Svg.Styled.text node.characters)
            |> withVisibility componentName config.propertyExpressions node.defaultShapeTraits.isLayerTrait.componentPropertyReferences
            |> List.singleton
            |> Elm.list
        )
        |> List.singleton


toStyles : ColorMap -> TextNode -> List Elm.Expression
toStyles colorMap node =
    TypeStyle.toStyles colorMap node.style
        ++ HasGeometryTrait.toStyles colorMap node.defaultShapeTraits.hasGeometryTrait
        ++ HasBlendModeAndOpacityTrait.toStyles node.defaultShapeTraits.hasBlendModeAndOpacityTrait


toAlignmentBaseline : TypeStyleTextAlignVertical -> Elm.Expression
toAlignmentBaseline align =
    Attributes.dominantBaseline <|
        case align of
            TypeStyleTextAlignVerticalTOP ->
                "hanging"

            TypeStyleTextAlignVerticalCENTER ->
                "middle"

            TypeStyleTextAlignVerticalBOTTOM ->
                "bottom"


toAttributes : TextNode -> List Elm.Expression
toAttributes node =
    []
        |> mm (toCoords node.style) (Just node.defaultShapeTraits.absoluteBoundingBox)
        |> m toTextAnchor node.style.textAlignHorizontal
        |> m toAlignmentBaseline node.style.textAlignVertical


toTextAnchor : TypeStyleTextAlignHorizontal -> Elm.Expression
toTextAnchor align =
    Attributes.textAnchor <|
        case align of
            TypeStyleTextAlignHorizontalLEFT ->
                "start"

            TypeStyleTextAlignHorizontalRIGHT ->
                "end"

            TypeStyleTextAlignHorizontalCENTER ->
                "middle"

            TypeStyleTextAlignHorizontalJUSTIFIED ->
                "start"


toCoords : TypeStyle -> Rectangle -> List Elm.Expression
toCoords ts bb =
    let
        ax =
            case ts.textAlignHorizontal of
                Just TypeStyleTextAlignHorizontalLEFT ->
                    0

                Just TypeStyleTextAlignHorizontalRIGHT ->
                    bb.width

                Just TypeStyleTextAlignHorizontalCENTER ->
                    bb.width / 2

                Just TypeStyleTextAlignHorizontalJUSTIFIED ->
                    0

                Nothing ->
                    0

        ay =
            case ts.textAlignVertical of
                Just TypeStyleTextAlignVerticalTOP ->
                    0

                Just TypeStyleTextAlignVerticalCENTER ->
                    bb.height / 2

                Just TypeStyleTextAlignVerticalBOTTOM ->
                    bb.height

                Nothing ->
                    0
    in
    [ bb.x + ax |> String.fromFloat |> Attributes.x
    , bb.y + ay |> String.fromFloat |> Attributes.y
    ]

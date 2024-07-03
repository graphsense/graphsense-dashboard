module Generate.Svg.TextNode exposing (..)

import Api.Raw exposing (..)
import Dict
import Elm
import Elm.Op
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes as Attributes
import Generate.Common.TextNode exposing (getName)
import Generate.Svg.DefaultShapeTraits as DefaultShapeTraits
import Generate.Svg.TypeStyle as TypeStyle
import Generate.Util exposing (getElementAttributes, m, mm)
import Types exposing (Config)


toExpressions : Config -> TextNode -> List Elm.Expression
toExpressions config node =
    Gen.Svg.Styled.call_.text_
        (getName node
            |> getElementAttributes config
            |> Elm.Op.append
                ((toCss node |> Attributes.css)
                    :: toAttributes node
                    |> Elm.list
                )
        )
        (node.defaultShapeTraits.isLayerTrait.componentPropertyReferences
            |> Maybe.andThen (Dict.get "characters")
            |> Maybe.andThen (\ref -> Dict.get ref config.propertyExpressions)
            |> Maybe.map Gen.Svg.Styled.call_.text
            |> Maybe.withDefault (Gen.Svg.Styled.text node.characters)
            |> List.singleton
            |> Elm.list
            |> Gen.Svg.Styled.call_.tspan (tspanAttributes node |> Elm.list)
            |> List.singleton
            |> Elm.list
        )
        |> List.singleton


toCss : TextNode -> List Elm.Expression
toCss node =
    TypeStyle.toCss node.style
        ++ DefaultShapeTraits.toCss node.defaultShapeTraits


tspanAttributes : TextNode -> List Elm.Expression
tspanAttributes node =
    []
        |> m toAlignmentBaseline node.style.textAlignVertical


toAlignmentBaseline : TypeStyleTextAlignVertical -> Elm.Expression
toAlignmentBaseline align =
    Attributes.alignmentBaseline <|
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

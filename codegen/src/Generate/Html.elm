module Generate.Html exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (uncurry)
import Dict
import Elm
import Elm.Op
import Gen.Css as Css
import Gen.Html.Styled
import Gen.Html.Styled.Attributes as Attributes
import Gen.Maybe
import Generate.Common exposing (hasMainComponentProperty, hasVariantProperty, wrapInSvg)
import Generate.Common.DefaultShapeTraits
import Generate.Common.FrameTraits
import Generate.Html.ComponentNode as ComponentNode
import Generate.Html.DefaultShapeTraits as DefaultShapeTraits
import Generate.Html.FrameTraits as FrameTraits
import Generate.Html.TextNode as TextNode
import Generate.Svg as Svg
import Generate.Svg.EllipseNode
import Generate.Svg.LineNode
import Generate.Svg.RectangleNode
import Generate.Svg.VectorNode
import Generate.Util exposing (callStyles, getElementAttributes, sanitize, withVisibility)
import Maybe.Extra
import RecordSetter exposing (..)
import Types exposing (ColorMap, Config, Styles)


subcanvasNodeToExpressions : Config -> String -> SubcanvasNode -> List Elm.Expression
subcanvasNodeToExpressions config name node =
    case node of
        SubcanvasNodeTextNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n then
                []

            else
                TextNode.toExpressions config name n

        SubcanvasNodeGroupNode n ->
            if Generate.Common.FrameTraits.isHidden n then
                []

            else
                withFrameTraitsNodeToExpression config name name n.frameTraits
                    |> List.singleton

        SubcanvasNodeFrameNode n ->
            if Generate.Common.FrameTraits.isHidden n then
                []

            else
                Elm.get (Generate.Common.FrameTraits.getName n.frameTraits) config.instances
                    |> Gen.Maybe.withDefault
                        (withFrameTraitsNodeToExpression config name name n.frameTraits)
                    |> List.singleton

        SubcanvasNodeInstanceNode n ->
            if Generate.Common.FrameTraits.isHidden n then
                []

            else
                let
                    config_ =
                        { config
                            | instanceName =
                                -- store the name of the highest level instance node
                                if config.instanceName == "" then
                                    Generate.Common.FrameTraits.getName n.frameTraits

                                else
                                    config.instanceName
                        }
                in
                instanceNodeToExpressions config_ name n

        SubcanvasNodeRectangleNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n.rectangularShapeTraits then
                []

            else
                Generate.Svg.RectangleNode.toExpressions config name n
                    |> DefaultShapeTraits.toExpressions config n.rectangularShapeTraits.defaultShapeTraits

        SubcanvasNodeEllipseNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n then
                []

            else
                Generate.Svg.EllipseNode.toExpressions config name n
                    |> DefaultShapeTraits.toExpressions config n.defaultShapeTraits

        SubcanvasNodeVectorNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n.cornerRadiusShapeTraits then
                []

            else
                Generate.Svg.VectorNode.toExpressions config name n.cornerRadiusShapeTraits
                    |> DefaultShapeTraits.toExpressions config n.cornerRadiusShapeTraits.defaultShapeTraits

        SubcanvasNodeLineNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n then
                []

            else
                Generate.Svg.LineNode.toExpressions config name n
                    |> DefaultShapeTraits.toExpressions config n.defaultShapeTraits

        _ ->
            []


frameTraitsToExpression : Config -> String -> FrameTraits -> Elm.Expression
frameTraitsToExpression config componentName node =
    if Generate.Common.FrameTraits.isList { frameTraits = node } then
        config.children
            |> Elm.get (Generate.Common.FrameTraits.getName node)

    else
        let
            config_ =
                { config
                    | positionRelatively =
                        if layoutIsAbsolute node then
                            Just { x = node.absoluteBoundingBox.x, y = node.absoluteBoundingBox.y }

                        else
                            Nothing
                }
        in
        node.children
            |> List.map (subcanvasNodeToExpressions config_ componentName)
            |> List.concat
            |> Elm.list


layoutIsAbsolute : FrameTraits -> Bool
layoutIsAbsolute node =
    (node.layoutGrow
        == Nothing
        || node.layoutGrow
        == Just LayoutGrow0
    )
        && (node.layoutMode
                == Just LayoutModeNONE
                || node.layoutMode
                == Nothing
           )


isSvgChild : SubcanvasNode -> Bool
isSvgChild child =
    case child of
        SubcanvasNodeRectangleNode _ ->
            True

        SubcanvasNodeVectorNode _ ->
            True

        SubcanvasNodeEllipseNode _ ->
            True

        SubcanvasNodeLineNode _ ->
            True

        SubcanvasNodeGroupNode n ->
            List.all isSvgChild n.frameTraits.children

        _ ->
            False


withFrameTraitsNodeToExpression : Config -> String -> String -> FrameTraits -> Elm.Expression
withFrameTraitsNodeToExpression config componentName componentNameForChildren node =
    let
        name =
            Generate.Common.FrameTraits.getName node

        hasOnlySvgChildren =
            not (List.isEmpty node.children) && List.all isSvgChild node.children

        frame =
            if hasOnlySvgChildren then
                Svg.frameTraitsToExpressions config componentNameForChildren node
                    |> wrapInSvg config name node

            else
                Gen.Html.Styled.call_.div
                    (getElementAttributes config name
                        |> Elm.Op.append
                            (Generate.Common.DefaultShapeTraits.positionRelatively config node
                                ++ cssDimensionsIfAbsolute node
                                |> Elm.list
                                |> Elm.Op.append (callStyles config name)
                                |> Attributes.call_.css
                                |> List.singleton
                                |> Elm.list
                            )
                    )
                    (frameTraitsToExpression config componentNameForChildren node)
    in
    frame
        |> withVisibility componentName config.propertyExpressions node.isLayerTrait.componentPropertyReferences


cssDimensionsIfAbsolute : FrameTraits -> List Elm.Expression
cssDimensionsIfAbsolute node =
    if layoutIsAbsolute node then
        [ node.absoluteBoundingBox.width |> Css.px |> Css.width
        , node.absoluteBoundingBox.height |> Css.px |> Css.height
        ]

    else
        []


instanceNodeToExpressions : Config -> String -> InstanceNode -> List Elm.Expression
instanceNodeToExpressions config parentName node =
    let
        name =
            Generate.Common.FrameTraits.getName node.frameTraits

        subNameId =
            if node.componentProperties /= Nothing then
                name

            else
                parentName
    in
    if Generate.Common.FrameTraits.isHidden node then
        []

    else
        (node.frameTraits.isLayerTrait.componentPropertyReferences
            |> Maybe.andThen (Dict.get "mainComponent")
            |> Maybe.andThen
                (\ref ->
                    Dict.get parentName config.propertyExpressions
                        |> Maybe.andThen (Dict.get ref)
                )
            |> Maybe.Extra.orElseLazy
                (\_ ->
                    Dict.get (Generate.Common.FrameTraits.getName node.frameTraits |> sanitize) config.propertyExpressions
                        |> Maybe.andThen (Dict.get "variant")
                )
            |> Maybe.Extra.withDefaultLazy
                (\_ ->
                    Elm.get name config.instances
                        |> Gen.Maybe.withDefault
                            (withFrameTraitsNodeToExpression config name subNameId node.frameTraits)
                )
        )
            |> withVisibility parentName config.propertyExpressions node.frameTraits.isLayerTrait.componentPropertyReferences
            |> List.singleton


instanceNodeToStyles : InstanceNode -> List Elm.Expression
instanceNodeToStyles _ =
    []


subcanvasNodeToStyles : ColorMap -> SubcanvasNode -> List Styles
subcanvasNodeToStyles colorMap node =
    case node of
        SubcanvasNodeTextNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n then
                []

            else
                TextNode.toStyles colorMap n
                    |> List.singleton

        SubcanvasNodeGroupNode n ->
            if Generate.Common.FrameTraits.isHidden n then
                []

            else
                withFrameTraitsNodeToStyles colorMap n
                    |> uncurry (::)

        SubcanvasNodeFrameNode n ->
            if Generate.Common.FrameTraits.isHidden n then
                []

            else
                withFrameTraitsNodeToStyles colorMap n
                    |> uncurry (::)

        SubcanvasNodeInstanceNode n ->
            if Generate.Common.FrameTraits.isHidden n then
                []

            else if hasVariantProperty n || hasMainComponentProperty n then
                []

            else
                withFrameTraitsNodeToStyles colorMap n
                    |> uncurry (::)

        SubcanvasNodeRectangleNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n.rectangularShapeTraits then
                []

            else
                Generate.Svg.RectangleNode.toStyles colorMap n
                    |> List.singleton

        SubcanvasNodeVectorNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n.cornerRadiusShapeTraits then
                []

            else
                Generate.Svg.VectorNode.toStyles colorMap n.cornerRadiusShapeTraits
                    |> List.singleton

        SubcanvasNodeLineNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n then
                []

            else
                Generate.Svg.LineNode.toStyles colorMap n
                    |> List.singleton

        SubcanvasNodeEllipseNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n then
                []

            else
                Generate.Svg.EllipseNode.toStyles colorMap n
                    |> List.singleton

        _ ->
            []


componentNodeToStyles : ColorMap -> FrameTraits -> ( Styles, List Styles )
componentNodeToStyles colorMap node =
    ( ComponentNode.toStyles colorMap node
    , node.children
        |> List.map (subcanvasNodeToStyles colorMap)
        |> List.concat
    )


withFrameTraitsNodeToStyles : ColorMap -> { a | frameTraits : FrameTraits } -> ( Styles, List Styles )
withFrameTraitsNodeToStyles colorMap node =
    ( FrameTraits.toStyles colorMap node.frameTraits
    , if Generate.Common.FrameTraits.isList node then
        []

      else
        node.frameTraits.children
            |> List.map (subcanvasNodeToStyles colorMap)
            |> List.concat
    )

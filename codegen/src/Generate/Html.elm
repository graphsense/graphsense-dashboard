module Generate.Html exposing (..)

{-| -}

import Api.Raw exposing (..)
import Basics.Extra exposing (uncurry)
import Config exposing (showId)
import Dict exposing (Dict)
import Elm
import Elm.Annotation as Annotation
import Elm.Op
import Gen.Css as Css
import Gen.Html.Styled
import Gen.Html.Styled.Attributes as Attributes
import Gen.Maybe
import Generate.Common as Common exposing (hasMainComponentProperty, hasVariantProperty, wrapInSvg)
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
import Generate.Util exposing (addIdAttribute, callStyles, detailsToDeclaration, getElementAttributes, sanitize, withVisibility)
import Maybe.Extra
import RecordSetter exposing (..)
import Types exposing (ColorMap, Config, Details)


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
                withFrameTraitsNodeToExpression config name name n
                    |> List.singleton

        SubcanvasNodeFrameNode n ->
            if Generate.Common.FrameTraits.isHidden n then
                []

            else
                Elm.get (Generate.Common.FrameTraits.getName n) config.instances
                    |> Gen.Maybe.withDefault
                        (withFrameTraitsNodeToExpression config name name n)
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
                                    Generate.Common.FrameTraits.getName n

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
                    |> DefaultShapeTraits.toExpressions config n.rectangularShapeTraits

        SubcanvasNodeEllipseNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n then
                []

            else
                Generate.Svg.EllipseNode.toExpressions config name n
                    |> DefaultShapeTraits.toExpressions config n

        SubcanvasNodeVectorNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n.cornerRadiusShapeTraits then
                []

            else
                Generate.Svg.VectorNode.toExpressions config name n.cornerRadiusShapeTraits
                    |> DefaultShapeTraits.toExpressions config n.cornerRadiusShapeTraits

        SubcanvasNodeLineNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n then
                []

            else
                Generate.Svg.LineNode.toExpressions config name n
                    |> DefaultShapeTraits.toExpressions config n

        _ ->
            []


componentNodeToDeclarations : ColorMap -> String -> Dict String (Dict String ComponentPropertyType) -> ComponentNode -> List Elm.Declaration
componentNodeToDeclarations colorMap parentName parentProperties node =
    let
        ( details, descendantsDetails ) =
            componentNodeToDetails colorMap node

        names =
            details.name
                :: List.map .name descendantsDetails

        properties =
            Common.componentNodeToProperties details.name node
                |> Dict.union parentProperties

        propertiesType =
            properties
                |> Common.propertiesType Gen.Html.Styled.annotation_.html

        declarationName =
            parentName ++ " " ++ details.name

        propertiesParam =
            ( "properties"
            , Just propertiesType
            )

        instancesType =
            names
                |> List.map
                    (\n ->
                        ( n
                        , Gen.Html.Styled.annotation_.html (Annotation.var "msg")
                            |> Annotation.maybe
                        )
                    )
                |> Annotation.record

        instancesParam =
            ( "instances"
            , instancesType
                |> Just
            )

        namesWithList =
            names
                |> List.filter Generate.Common.FrameTraits.nameIsList

        childrenType =
            namesWithList
                |> List.map
                    (\n ->
                        ( n
                        , Gen.Html.Styled.annotation_.html (Annotation.var "msg")
                            |> Annotation.list
                        )
                    )
                |> Annotation.record

        childrenParam =
            ( "children"
            , childrenType
                |> Just
            )

        attributesType =
            names
                |> List.map (\n -> ( n, Gen.Html.Styled.annotation_.attribute (Annotation.var "msg") |> Annotation.list ))
                |> Annotation.record

        attributesParam =
            ( "attributes"
            , attributesType
                |> Just
            )

        declarationNameWithInstances =
            declarationName ++ " with instances" |> sanitize

        declarationNameAttributes =
            declarationName ++ " attributes" |> sanitize

        declarationNameInstances =
            declarationName ++ " instances" |> sanitize
    in
    details
        :: descendantsDetails
        |> List.foldl
            (\md ->
                Dict.insert md.name md
            )
            Dict.empty
        |> Dict.values
        |> List.map (detailsToDeclaration parentName details.name)
        |> (++)
            [ propertiesType
                |> Elm.alias (declarationName ++ " properties" |> sanitize)
            , names
                |> Common.defaultAttributeConfig Gen.Html.Styled.annotation_.attribute
                |> Elm.declaration declarationNameAttributes
            , names
                |> Common.defaultInstancesConfig Gen.Html.Styled.annotation_.html
                |> Elm.declaration declarationNameInstances
            , if List.isEmpty namesWithList then
                Elm.fn3
                    attributesParam
                    instancesParam
                    propertiesParam
                    (\attributes instances properties_ ->
                        let
                            config =
                                { propertyExpressions =
                                    Common.propertiesToPropertyExpressions properties_ properties
                                , positionRelatively = Nothing
                                , attributes = attributes
                                , instances = instances
                                , children = Elm.record []
                                , colorMap = colorMap
                                , parentName = parentName
                                , componentName = details.name
                                , instanceName = ""
                                , showId = showId
                                }
                        in
                        withFrameTraitsNodeToExpression config details.name details.name node
                    )
                    |> Elm.withType
                        (Annotation.function
                            [ attributesType, instancesType, propertiesType ]
                            (Gen.Html.Styled.annotation_.html (Annotation.var "msg"))
                        )
                    |> Elm.declaration declarationNameWithInstances

              else
                Elm.fn4
                    attributesParam
                    instancesParam
                    childrenParam
                    propertiesParam
                    (\attributes instances children properties_ ->
                        let
                            config =
                                { propertyExpressions =
                                    Common.propertiesToPropertyExpressions properties_ properties
                                , positionRelatively = Nothing
                                , attributes = attributes
                                , instances = instances
                                , children = children
                                , colorMap = colorMap
                                , parentName = parentName
                                , componentName = details.name
                                , instanceName = ""
                                , showId = showId
                                }
                        in
                        withFrameTraitsNodeToExpression config details.name details.name node
                    )
                    |> Elm.withType
                        (Annotation.function
                            [ attributesType, instancesType, childrenType, propertiesType ]
                            (Gen.Html.Styled.annotation_.html (Annotation.var "msg"))
                        )
                    |> Elm.declaration declarationNameWithInstances
            , if List.isEmpty namesWithList then
                Elm.fn
                    propertiesParam
                    (\properties_ ->
                        Elm.apply
                            (Elm.value
                                { importFrom = []
                                , name = declarationNameWithInstances
                                , annotation = Nothing
                                }
                            )
                            [ Elm.value
                                { importFrom = []
                                , name = declarationNameAttributes
                                , annotation = Nothing
                                }
                            , Elm.value
                                { importFrom = []
                                , name = declarationNameInstances
                                , annotation = Nothing
                                }
                            , properties_
                            ]
                    )
                    |> Elm.withType
                        (Annotation.function
                            [ propertiesType ]
                            (Gen.Html.Styled.annotation_.html (Annotation.var "msg"))
                        )
                    |> Elm.declaration (sanitize declarationName)

              else
                Elm.fn2
                    childrenParam
                    propertiesParam
                    (\children properties_ ->
                        Elm.apply
                            (Elm.value
                                { importFrom = []
                                , name = declarationNameWithInstances
                                , annotation = Nothing
                                }
                            )
                            [ Elm.value
                                { importFrom = []
                                , name = declarationNameAttributes
                                , annotation = Nothing
                                }
                            , Elm.value
                                { importFrom = []
                                , name = declarationNameInstances
                                , annotation = Nothing
                                }
                            , children
                            , properties_
                            ]
                    )
                    |> Elm.withType
                        (Annotation.function
                            [ childrenType, propertiesType ]
                            (Gen.Html.Styled.annotation_.html (Annotation.var "msg"))
                        )
                    |> Elm.declaration (sanitize declarationName)
            , if List.isEmpty namesWithList then
                Elm.fn2
                    attributesParam
                    propertiesParam
                    (\attributes properties_ ->
                        Elm.apply
                            (Elm.value
                                { importFrom = []
                                , name = declarationNameWithInstances
                                , annotation = Nothing
                                }
                            )
                            [ attributes
                            , Elm.value
                                { importFrom = []
                                , name = declarationNameInstances
                                , annotation = Nothing
                                }
                            , properties_
                            ]
                    )
                    |> Elm.withType
                        (Annotation.function
                            [ attributesType, propertiesType ]
                            (Gen.Html.Styled.annotation_.html (Annotation.var "msg"))
                        )
                    |> Elm.declaration (sanitize <| declarationName ++ " with attributes")

              else
                Elm.fn3
                    attributesParam
                    childrenParam
                    propertiesParam
                    (\attributes children properties_ ->
                        Elm.apply
                            (Elm.value
                                { importFrom = []
                                , name = declarationNameWithInstances
                                , annotation = Nothing
                                }
                            )
                            [ attributes
                            , Elm.value
                                { importFrom = []
                                , name = declarationNameInstances
                                , annotation = Nothing
                                }
                            , children
                            , properties_
                            ]
                    )
                    |> Elm.withType
                        (Annotation.function
                            [ attributesType, childrenType, propertiesType ]
                            (Gen.Html.Styled.annotation_.html (Annotation.var "msg"))
                        )
                    |> Elm.declaration (sanitize <| declarationName ++ " with attributes")
            ]


frameTraitsToExpression : Config -> String -> FrameTraits -> Elm.Expression
frameTraitsToExpression config componentName node =
    if Generate.Common.FrameTraits.isList { frameTraits = node } then
        config.children
            |> Elm.get (Generate.Common.FrameTraits.getName { frameTraits = node })

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


withFrameTraitsNodeToExpression : Config -> String -> String -> { a | frameTraits : FrameTraits } -> Elm.Expression
withFrameTraitsNodeToExpression config componentName componentNameForChildren node =
    let
        name =
            Generate.Common.FrameTraits.getName node

        hasOnlySvgChildren =
            not (List.isEmpty node.frameTraits.children) && List.all isSvgChild node.frameTraits.children

        frame =
            if hasOnlySvgChildren then
                Svg.frameTraitsToExpressions config componentNameForChildren node.frameTraits
                    |> wrapInSvg config name node.frameTraits

            else
                Gen.Html.Styled.call_.div
                    (getElementAttributes config name
                        |> Elm.Op.append
                            (Generate.Common.DefaultShapeTraits.positionRelatively config node.frameTraits
                                ++ cssDimensionsIfAbsolute node.frameTraits
                                |> Elm.list
                                |> Elm.Op.append (callStyles config name)
                                |> Attributes.call_.css
                                |> List.singleton
                                |> Elm.list
                            )
                    )
                    (frameTraitsToExpression config componentNameForChildren node.frameTraits)
    in
    frame
        |> withVisibility componentName config.propertyExpressions node.frameTraits.isLayerTrait.componentPropertyReferences


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
            Generate.Common.FrameTraits.getName node

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
                    Dict.get (Generate.Common.FrameTraits.getName node |> sanitize) config.propertyExpressions
                        |> Maybe.andThen (Dict.get "variant")
                )
            |> Maybe.Extra.withDefaultLazy
                (\_ ->
                    Elm.get name config.instances
                        |> Gen.Maybe.withDefault
                            (withFrameTraitsNodeToExpression config name subNameId node)
                )
        )
            |> withVisibility parentName config.propertyExpressions node.frameTraits.isLayerTrait.componentPropertyReferences
            |> List.singleton


instanceNodeToStyles : InstanceNode -> List Elm.Expression
instanceNodeToStyles _ =
    []


subcanvasNodeToDetails : ColorMap -> SubcanvasNode -> List Details
subcanvasNodeToDetails colorMap node =
    case node of
        SubcanvasNodeComponentNode n ->
            if Generate.Common.FrameTraits.isHidden n then
                []

            else
                withFrameTraitsNodeToDetails colorMap n
                    |> uncurry (::)

        SubcanvasNodeComponentSetNode n ->
            if Generate.Common.FrameTraits.isHidden n then
                []

            else
                withFrameTraitsNodeToDetails colorMap n
                    |> uncurry (::)

        SubcanvasNodeTextNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n then
                []

            else
                TextNode.toDetails colorMap n
                    |> List.singleton

        SubcanvasNodeGroupNode n ->
            if Generate.Common.FrameTraits.isHidden n then
                []

            else
                withFrameTraitsNodeToDetails colorMap n
                    |> uncurry (::)

        SubcanvasNodeFrameNode n ->
            if Generate.Common.FrameTraits.isHidden n then
                []

            else
                withFrameTraitsNodeToDetails colorMap n
                    |> uncurry (::)

        SubcanvasNodeInstanceNode n ->
            if Generate.Common.FrameTraits.isHidden n then
                []

            else if hasVariantProperty n || hasMainComponentProperty n then
                []

            else
                withFrameTraitsNodeToDetails colorMap n
                    |> uncurry (::)
                    |> List.map (s_instanceName (Generate.Common.FrameTraits.getName n))

        SubcanvasNodeRectangleNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n.rectangularShapeTraits then
                []

            else
                Generate.Svg.RectangleNode.toDetails colorMap n
                    |> List.singleton

        SubcanvasNodeVectorNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n.cornerRadiusShapeTraits then
                []

            else
                Generate.Svg.VectorNode.toDetails colorMap n.cornerRadiusShapeTraits
                    |> List.singleton

        SubcanvasNodeLineNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n then
                []

            else
                Generate.Svg.LineNode.toDetails colorMap n
                    |> List.singleton

        SubcanvasNodeEllipseNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n then
                []

            else
                Generate.Svg.EllipseNode.toDetails colorMap n
                    |> List.singleton

        _ ->
            []


componentNodeToDetails : ColorMap -> ComponentNode -> ( Details, List Details )
componentNodeToDetails colorMap node =
    ( FrameTraits.toDetails colorMap node
        |> s_styles (ComponentNode.toStyles colorMap node)
    , node.frameTraits.children
        |> List.map (subcanvasNodeToDetails colorMap)
        |> List.concat
    )


withFrameTraitsNodeToDetails : ColorMap -> { a | frameTraits : FrameTraits } -> ( Details, List Details )
withFrameTraitsNodeToDetails colorMap node =
    ( FrameTraits.toDetails colorMap node
    , if Generate.Common.FrameTraits.isList node then
        []

      else
        node.frameTraits.children
            |> List.map (subcanvasNodeToDetails colorMap)
            |> List.concat
    )

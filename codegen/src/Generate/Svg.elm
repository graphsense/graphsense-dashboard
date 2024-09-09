module Generate.Svg exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (uncurry)
import Dict
import Elm
import Elm.Annotation as Annotation
import Elm.Op
import Gen.Maybe
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes as Attributes
import Generate.Common as Common exposing (adjustBoundingBoxes)
import Generate.Common.FrameTraits
import Generate.Svg.DefaultShapeTraits as DefaultShapeTraits
import Generate.Svg.EllipseNode as EllipseNode
import Generate.Svg.FrameTraits as FrameTraits
import Generate.Svg.RectangleNode as RectangleNode
import Generate.Svg.TextNode as TextNode
import Generate.Util exposing (detailsToDeclaration, getElementAttributes, sanitize, toTranslate, withVisibility)
import Maybe.Extra
import RecordSetter exposing (..)
import Types exposing (Config, Details)


subcanvasNodeComponentsToDeclarations : String -> SubcanvasNode -> List Elm.Declaration
subcanvasNodeComponentsToDeclarations parentName node =
    case node of
        SubcanvasNodeComponentNode n ->
            adjustBoundingBoxes n
                |> Common.adjustNames
                |> componentNodeToDeclarations parentName

        SubcanvasNodeComponentSetNode n ->
            n.frameTraits.children
                |> List.map
                    (subcanvasNodeComponentsToDeclarations
                        (Generate.Common.FrameTraits.getName n)
                    )
                |> List.concat

        _ ->
            []


subcanvasNodeToExpressions : Config -> String -> SubcanvasNode -> List Elm.Expression
subcanvasNodeToExpressions config name node =
    case node of
        SubcanvasNodeTextNode n ->
            TextNode.toExpressions config name n

        SubcanvasNodeEllipseNode n ->
            EllipseNode.toExpressions config name n

        SubcanvasNodeGroupNode n ->
            withFrameTraitsNodeToExpressions config name n

        SubcanvasNodeFrameNode n ->
            withFrameTraitsNodeToExpressions config name n

        SubcanvasNodeInstanceNode n ->
            instanceNodeToExpressions config name n

        SubcanvasNodeVectorNode n ->
            DefaultShapeTraits.toExpressions config name n.cornerRadiusShapeTraits

        SubcanvasNodeRectangleNode n ->
            RectangleNode.toExpressions config name n

        SubcanvasNodeLineNode n ->
            DefaultShapeTraits.toExpressions config name n

        _ ->
            []


subcanvasNodeToDetails : SubcanvasNode -> List Details
subcanvasNodeToDetails node =
    case node of
        SubcanvasNodeComponentNode n ->
            withFrameTraitsNodeToDetails n
                |> uncurry (::)

        SubcanvasNodeComponentSetNode n ->
            withFrameTraitsNodeToDetails n
                |> uncurry (::)

        SubcanvasNodeTextNode n ->
            TextNode.toDetails n
                |> List.singleton

        SubcanvasNodeEllipseNode n ->
            DefaultShapeTraits.toDetails n
                |> List.singleton

        SubcanvasNodeGroupNode n ->
            withFrameTraitsNodeToDetails n
                |> uncurry (::)

        SubcanvasNodeFrameNode n ->
            withFrameTraitsNodeToDetails n
                |> uncurry (::)

        SubcanvasNodeInstanceNode n ->
            withFrameTraitsNodeToDetails n
                |> uncurry (::)
                |> List.map (s_instanceName (Generate.Common.FrameTraits.getName n))

        SubcanvasNodeRectangleNode n ->
            RectangleNode.toDetails n
                |> List.singleton

        SubcanvasNodeVectorNode n ->
            DefaultShapeTraits.toDetails n.cornerRadiusShapeTraits
                |> List.singleton

        SubcanvasNodeLineNode n ->
            DefaultShapeTraits.toDetails n
                |> List.singleton

        _ ->
            []


componentNodeToDeclarations : String -> ComponentNode -> List Elm.Declaration
componentNodeToDeclarations parentName node =
    let
        ( details, descendantsDetails ) =
            componentNodeToDetails node

        names =
            details.name
                :: List.map .name descendantsDetails

        properties =
            Common.componentNodeToProperties details.name node

        attributesParamName =
            "childrenAttributes"

        propertiesParamName =
            "properties"

        childrenAttributesType =
            names
                |> List.map (\n -> ( n, Gen.Svg.Styled.annotation_.attribute (Annotation.var "msg") |> Annotation.list ))
                |> Annotation.record

        attributesParam =
            ( "attributes", Nothing )

        childrenAttributesParam =
            ( attributesParamName
            , childrenAttributesType
                |> Just
            )

        propertiesType =
            properties |> Common.propertiesType Gen.Svg.Styled.annotation_.svg

        propertiesParam =
            ( propertiesParamName
            , propertiesType |> Just
            )

        instancesType =
            names
                |> List.map
                    (\n ->
                        ( n
                        , Gen.Svg.Styled.annotation_.svg (Annotation.var "msg")
                            |> Annotation.maybe
                        )
                    )
                |> Annotation.record

        attributesType =
            Gen.Svg.Styled.annotation_.attribute (Annotation.var "msg") |> Annotation.list

        instancesParam =
            ( "instances"
            , instancesType
                |> Just
            )

        declarationName =
            parentName ++ " " ++ details.name |> sanitize

        declarationNameWithInstances =
            declarationName ++ " with instances" |> sanitize

        declarationNameWithAttributes =
            declarationName ++ " with attributes" |> sanitize

        declarationNameWithInstancesSvg =
            declarationNameWithInstances ++ " svg" |> sanitize

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
            [ properties
                |> Common.propertiesType Gen.Svg.Styled.annotation_.svg
                |> Elm.alias (declarationName ++ " properties" |> sanitize)
            , names
                |> Common.defaultAttributeConfig Gen.Svg.Styled.annotation_.attribute
                |> Elm.declaration declarationNameAttributes
            , names
                |> Common.defaultInstancesConfig Gen.Svg.Styled.annotation_.svg
                |> Elm.declaration declarationNameInstances
            , Elm.fn3
                childrenAttributesParam
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
                            }
                    in
                    Gen.Svg.Styled.call_.g
                        (getElementAttributes config details.name)
                        (frameTraitsToExpressions config details.name node.frameTraits
                            |> Elm.list
                        )
                )
                |> Elm.withType
                    (Annotation.function
                        [ childrenAttributesType, instancesType, propertiesType ]
                        (Gen.Svg.Styled.annotation_.svg (Annotation.var "msg"))
                    )
                |> Elm.declaration declarationNameWithInstances
            , Elm.fn
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
                        (Gen.Svg.Styled.annotation_.svg (Annotation.var "msg"))
                    )
                |> Elm.declaration (sanitize declarationName)
            , Elm.fn2
                childrenAttributesParam
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
                        [ childrenAttributesType, propertiesType ]
                        (Gen.Svg.Styled.annotation_.svg (Annotation.var "msg"))
                    )
                |> Elm.declaration declarationNameWithAttributes

            -- SVG VARIANTS
            , Elm.fn4
                attributesParam
                childrenAttributesParam
                instancesParam
                propertiesParam
                (\attributes_ childrenAttributes instances properties_ ->
                    Elm.apply
                        (Elm.value
                            { importFrom = []
                            , name = declarationNameWithInstances
                            , annotation = Nothing
                            }
                        )
                        [ childrenAttributes
                        , instances
                        , properties_
                        ]
                        |> List.singleton
                        |> Elm.list
                        |> Gen.Svg.Styled.call_.svg
                            (attributes_
                                |> Elm.Op.append
                                    ([ max 1 details.bbox.width
                                        |> String.fromFloat
                                        |> Attributes.width
                                     , max 1 details.bbox.height
                                        |> String.fromFloat
                                        |> Attributes.height
                                     ]
                                        |> Elm.list
                                    )
                            )
                )
                |> Elm.withType
                    (Annotation.function
                        [ attributesType
                        , childrenAttributesType
                        , instancesType
                        , propertiesType
                        ]
                        (Gen.Svg.Styled.annotation_.svg (Annotation.var "msg"))
                    )
                |> Elm.declaration declarationNameWithInstancesSvg
            , Elm.fn2
                attributesParam
                propertiesParam
                (\attributes properties_ ->
                    Elm.apply
                        (Elm.value
                            { importFrom = []
                            , name = declarationNameWithInstancesSvg
                            , annotation = Nothing
                            }
                        )
                        [ attributes
                        , Elm.value
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
                        [ attributesType, propertiesType ]
                        (Gen.Svg.Styled.annotation_.svg (Annotation.var "msg"))
                    )
                |> Elm.declaration (sanitize <| declarationName ++ " svg")
            , Elm.fn3
                attributesParam
                childrenAttributesParam
                propertiesParam
                (\attributes childrenAttributes properties_ ->
                    Elm.apply
                        (Elm.value
                            { importFrom = []
                            , name = declarationNameWithInstancesSvg
                            , annotation = Nothing
                            }
                        )
                        [ attributes
                        , childrenAttributes
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
                        [ attributesType, childrenAttributesType, propertiesType ]
                        (Gen.Svg.Styled.annotation_.svg (Annotation.var "msg"))
                    )
                |> Elm.declaration (sanitize <| declarationNameWithAttributes ++ " svg")
            ]


withFrameTraitsNodeToDetails : { a | frameTraits : FrameTraits } -> ( Details, List Details )
withFrameTraitsNodeToDetails node =
    ( FrameTraits.toDetails node
    , node.frameTraits.children
        |> List.map subcanvasNodeToDetails
        |> List.concat
    )


componentNodeToDetails : ComponentNode -> ( Details, List Details )
componentNodeToDetails =
    withFrameTraitsNodeToDetails


frameTraitsToExpressions : Config -> String -> FrameTraits -> List Elm.Expression
frameTraitsToExpressions config componentName node =
    node.children
        |> List.map (subcanvasNodeToExpressions config componentName)
        |> List.concat


withFrameTraitsNodeToExpressions : Config -> String -> { a | frameTraits : FrameTraits } -> List Elm.Expression
withFrameTraitsNodeToExpressions config componentName node =
    let
        name =
            Generate.Common.FrameTraits.getName node
    in
    Gen.Svg.Styled.call_.g
        (getElementAttributes config name)
        (frameTraitsToExpressions config componentName node.frameTraits
            |> Elm.list
        )
        |> withVisibility componentName config.propertyExpressions node.frameTraits.isLayerTrait.componentPropertyReferences
        |> List.singleton


instanceNodeToExpressions : Config -> String -> InstanceNode -> List Elm.Expression
instanceNodeToExpressions config parentName node =
    let
        coords =
            node.frameTraits.absoluteBoundingBox
                |> toTranslate
                |> Attributes.transform

        name =
            Generate.Common.FrameTraits.getName node

        subNameId =
            if node.componentProperties /= Nothing then
                name

            else
                parentName
    in
    Elm.get name config.instances
        |> Gen.Maybe.withDefault
            (node.frameTraits.isLayerTrait.componentPropertyReferences
                |> Maybe.andThen (Dict.get "mainComponent")
                |> Maybe.andThen
                    (\ref ->
                        Dict.get parentName config.propertyExpressions
                            |> Maybe.andThen (Dict.get ref)
                    )
                |> Maybe.map
                    (List.singleton
                        >> Elm.list
                        >> Gen.Svg.Styled.call_.g (Elm.list [ coords ])
                    )
                |> Maybe.Extra.withDefaultLazy
                    (\_ ->
                        Gen.Svg.Styled.call_.g
                            (getElementAttributes config name)
                            (frameTraitsToExpressions config subNameId node.frameTraits
                                |> Elm.list
                            )
                    )
            )
        |> withVisibility parentName config.propertyExpressions node.frameTraits.isLayerTrait.componentPropertyReferences
        |> List.singleton

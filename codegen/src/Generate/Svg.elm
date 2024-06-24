module Generate.Svg exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (flip, uncurry)
import Dict
import Elm
import Elm.Annotation as Annotation
import Gen.Svg.Styled
import Generate.Svg.DefaultShapeTraits as DefaultShapeTraits
import Generate.Svg.EllipseNode as EllipseNode
import Generate.Svg.FrameTraits as FrameTraits
import Generate.Svg.RectangleNode as RectangleNode
import Generate.Svg.TextNode as TextNode
import Generate.Svg.VectorNode as VectorNode
import Generate.Util exposing (getElementAttributes, toTranslate, withVisibility)
import RecordSetter exposing (s_children, s_frameTraits)
import Set
import String.Case exposing (toCamelCaseLower, toCamelCaseUpper)
import String.Extra
import Tuple exposing (mapFirst)
import Types exposing (Config, Metadata, OriginAdjust)


subcanvasNodeComponentsToDeclarations : SubcanvasNode -> List Elm.Declaration
subcanvasNodeComponentsToDeclarations node =
    case node of
        SubcanvasNodeComponentNode n ->
            adjustBoundingBoxes n
                |> componentNodeToDeclarations

        SubcanvasNodeComponentSetNode n ->
            componentSetNodeToDeclarations n

        _ ->
            []


adjustBoundingBoxes : ComponentNode -> ComponentNode
adjustBoundingBoxes node =
    let
        originAdjust =
            getOriginAdjust node
    in
    withFrameTraitsAdjustBoundingBox originAdjust node


subcanvasNodeToExpressions : Config -> SubcanvasNode -> List Elm.Expression
subcanvasNodeToExpressions config node =
    case node of
        SubcanvasNodeTextNode n ->
            TextNode.toExpressions config n

        SubcanvasNodeEllipseNode n ->
            EllipseNode.toExpressions config n

        SubcanvasNodeGroupNode n ->
            withFrameTraitsNodeToExpressions config n

        SubcanvasNodeInstanceNode n ->
            instanceNodeToExpressions config n

        SubcanvasNodeVectorNode n ->
            VectorNode.toExpressions config n

        SubcanvasNodeRectangleNode n ->
            RectangleNode.toExpressions config n

        _ ->
            []


subcanvasNodeAdjustBoundingBox : OriginAdjust -> SubcanvasNode -> SubcanvasNode
subcanvasNodeAdjustBoundingBox adjust node =
    case node of
        SubcanvasNodeTextNode n ->
            DefaultShapeTraits.adjustBoundingBox adjust n
                |> SubcanvasNodeTextNode

        SubcanvasNodeEllipseNode n ->
            DefaultShapeTraits.adjustBoundingBox adjust n
                |> SubcanvasNodeEllipseNode

        SubcanvasNodeGroupNode n ->
            withFrameTraitsAdjustBoundingBox adjust n
                |> SubcanvasNodeGroupNode

        SubcanvasNodeInstanceNode n ->
            withFrameTraitsAdjustBoundingBox adjust n
                |> SubcanvasNodeInstanceNode

        SubcanvasNodeVectorNode n ->
            VectorNode.adjustBoundingBox adjust n
                |> SubcanvasNodeVectorNode

        SubcanvasNodeRectangleNode n ->
            RectangleNode.adjustBoundingBox adjust n
                |> SubcanvasNodeRectangleNode

        a ->
            a


withFrameTraitsAdjustBoundingBox : OriginAdjust -> { a | frameTraits : FrameTraits } -> { a | frameTraits : FrameTraits }
withFrameTraitsAdjustBoundingBox adjust node =
    node.frameTraits.children
        |> List.map (subcanvasNodeAdjustBoundingBox adjust)
        |> flip s_children node.frameTraits
        |> flip s_frameTraits node
        |> FrameTraits.adjustBoundingBox adjust


subcanvasNodeToMetadata : SubcanvasNode -> List Metadata
subcanvasNodeToMetadata node =
    case node of
        SubcanvasNodeComponentNode n ->
            withFrameTraitsNodeToMetadata n
                |> uncurry (::)

        SubcanvasNodeComponentSetNode n ->
            withFrameTraitsNodeToMetadata n
                |> uncurry (::)

        SubcanvasNodeTextNode n ->
            DefaultShapeTraits.toMetadata n
                |> List.singleton

        SubcanvasNodeEllipseNode n ->
            DefaultShapeTraits.toMetadata n
                |> List.singleton

        SubcanvasNodeGroupNode n ->
            withFrameTraitsNodeToMetadata n
                |> uncurry (::)

        SubcanvasNodeInstanceNode n ->
            withFrameTraitsNodeToMetadata n
                |> uncurry (::)

        SubcanvasNodeRectangleNode n ->
            n.rectangularShapeTraits
                |> DefaultShapeTraits.toMetadata
                |> List.singleton

        SubcanvasNodeVectorNode _ ->
            []

        _ ->
            []


withFrameTraitsNodeToMetadata : { a | frameTraits : FrameTraits } -> ( Metadata, List Metadata )
withFrameTraitsNodeToMetadata node =
    ( FrameTraits.toMetadata node, frameTraitsToMetadata node.frameTraits )


componentNodeToDeclarations : ComponentNode -> List Elm.Declaration
componentNodeToDeclarations node =
    let
        defaultAttributeConfig : List String -> Elm.Expression
        defaultAttributeConfig =
            Set.fromList
                >> Set.toList
                >> List.map
                    (\n ->
                        ( toCamelCaseLower n
                        , Elm.list []
                            |> Elm.withType
                                (Gen.Svg.Styled.annotation_.attribute
                                    (Annotation.var "msg")
                                    |> Annotation.list
                                )
                        )
                    )
                >> Elm.record

        ( metadata, descendantsMetadata ) =
            withFrameTraitsNodeToMetadata node

        names =
            metadata.name :: List.map .name descendantsMetadata

        formatName =
            String.Extra.leftOf "#" >> toCamelCaseLower

        properties =
            node.componentPropertiesTrait.componentPropertyDefinitions
                |> Maybe.map
                    (Dict.toList
                        >> List.map
                            (mapFirst
                                formatName
                            )
                        >> Dict.fromList
                    )
                |> Maybe.withDefault Dict.empty
    in
    metadata
        :: descendantsMetadata
        |> List.foldl
            (\md ->
                Dict.insert md.name md
            )
            Dict.empty
        |> Dict.values
        |> List.map (metadataToDeclaration metadata.name)
        |> (++)
            [ properties
                |> propertiesType
                |> Elm.alias (metadata.name ++ " properties" |> toCamelCaseUpper)
            , names
                |> defaultAttributeConfig
                |> Elm.declaration ("default " ++ metadata.name ++ " attributes" |> toCamelCaseLower)
            , Elm.fn2
                ( "attributes"
                , names
                    |> List.map (\n -> ( n, Gen.Svg.Styled.annotation_.attribute (Annotation.var "msg") |> Annotation.list ))
                    |> Annotation.record
                    |> Just
                )
                ( "properties"
                , Nothing
                )
                (\attributes children ->
                    let
                        config =
                            { propertyExpressions =
                                node.componentPropertiesTrait.componentPropertyDefinitions
                                    |> Maybe.map
                                        (Dict.map (\nam _ -> children |> Elm.get (formatName nam)))
                                    |> Maybe.withDefault Dict.empty
                            , attributes = attributes
                            }
                    in
                    Gen.Svg.Styled.call_.g
                        (getElementAttributes config metadata.name)
                        (frameTraitsToExpressions config node.frameTraits
                            |> Elm.list
                        )
                )
                |> Elm.declaration (toCamelCaseLower metadata.name)
            ]


metadataToDeclaration : String -> Metadata -> Elm.Declaration
metadataToDeclaration componentName metadata =
    let
        prefix =
            if componentName == metadata.name then
                componentName

            else
                componentName ++ " " ++ metadata.name
    in
    [ ( "x", Elm.float metadata.bbox.x )
    , ( "y", Elm.float metadata.bbox.y )
    , ( "width", Elm.float metadata.bbox.width )
    , ( "height", Elm.float metadata.bbox.height )
    ]
        |> Elm.record
        |> Elm.declaration (prefix ++ " dimensions" |> toCamelCaseLower)


getOriginAdjust : ComponentNode -> OriginAdjust
getOriginAdjust node =
    node.frameTraits.absoluteBoundingBox
        |> (\r ->
                { x = r.x
                , y = r.y
                }
           )


propertiesType : Dict.Dict String ComponentPropertyDefinition -> Annotation.Annotation
propertiesType =
    Dict.map
        (\_ def ->
            case def.type_ of
                ComponentPropertyTypeBOOLEAN ->
                    Annotation.bool

                ComponentPropertyTypeINSTANCESWAP ->
                    Gen.Svg.Styled.annotation_.svg (Annotation.var "msg")

                ComponentPropertyTypeTEXT ->
                    Annotation.string

                ComponentPropertyTypeVARIANT ->
                    Debug.todo "support variant"
        )
        >> Dict.toList
        >> Annotation.record


frameTraitsToMetadata : FrameTraits -> List Metadata
frameTraitsToMetadata node =
    node.children
        |> List.map subcanvasNodeToMetadata
        |> List.concat


componentSetNodeToDeclarations : ComponentSetNode -> List Elm.Declaration
componentSetNodeToDeclarations _ =
    Debug.todo "componentSetNodeToDeclarations"


frameTraitsToExpressions : Config -> FrameTraits -> List Elm.Expression
frameTraitsToExpressions config node =
    node.children
        |> List.map (subcanvasNodeToExpressions config)
        |> List.concat


withFrameTraitsNodeToExpressions : Config -> { a | frameTraits : FrameTraits } -> List Elm.Expression
withFrameTraitsNodeToExpressions config node =
    let
        { name } =
            FrameTraits.toMetadata node
    in
    Gen.Svg.Styled.call_.g
        (getElementAttributes config name)
        (frameTraitsToExpressions config node.frameTraits
            |> Elm.list
        )
        |> withVisibility config.propertyExpressions node.frameTraits.isLayerTrait.componentPropertyReferences
        |> List.singleton


instanceNodeToExpressions : Config -> InstanceNode -> List Elm.Expression
instanceNodeToExpressions config node =
    let
        coords =
            node.frameTraits.absoluteBoundingBox
                |> toTranslate
    in
    node.frameTraits.isLayerTrait.componentPropertyReferences
        |> Maybe.andThen (Dict.get "mainComponent")
        |> Maybe.andThen (\ref -> Dict.get ref config.propertyExpressions)
        |> Maybe.map
            (List.singleton
                >> Elm.list
                >> Gen.Svg.Styled.call_.g (Elm.list [ coords ])
                >> List.singleton
            )
        |> Maybe.withDefault
            (withFrameTraitsNodeToExpressions config node)

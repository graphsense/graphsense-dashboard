module Generate.Svg exposing (..)

import Api.Raw exposing (..)
import Dict
import Elm
import Elm.Annotation as Annotation
import Elm.Op
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes as Attributes
import Generate.Common as Common exposing (adjustBoundingBoxes)
import Generate.Common.FrameTraits as FrameTraits
import Generate.Svg.EllipseNode as EllipseNode
import Generate.Svg.FrameTraits as FrameTraits
import Generate.Svg.RectangleNode as RectangleNode
import Generate.Svg.TextNode as TextNode
import Generate.Svg.VectorNode as VectorNode
import Generate.Util exposing (getElementAttributes, metadataToDeclaration, sanitize, toTranslate, withVisibility)
import Set
import String.Case exposing (toCamelCaseLower, toCamelCaseUpper)
import String.Extra
import Tuple exposing (mapFirst)
import Types exposing (Config)


subcanvasNodeComponentsToDeclarations : SubcanvasNode -> List Elm.Declaration
subcanvasNodeComponentsToDeclarations node =
    case node of
        SubcanvasNodeComponentNode n ->
            adjustBoundingBoxes n
                |> componentNodeToDeclarations

        SubcanvasNodeComponentSetNode n ->
            n.frameTraits.children
                |> List.map subcanvasNodeComponentsToDeclarations
                |> List.concat

        _ ->
            []


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


componentNodeToDeclarations : ComponentNode -> List Elm.Declaration
componentNodeToDeclarations node =
    let
        ( metadata, descendantsMetadata ) =
            Common.withFrameTraitsNodeToMetadata node

        names =
            metadata.name
                :: List.map .name descendantsMetadata

        formatName =
            String.Extra.leftOf "#" >> sanitize

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

        defaultAttributeConfig : List String -> Elm.Expression
        defaultAttributeConfig =
            Set.fromList
                >> Set.toList
                >> List.map
                    (\n ->
                        ( sanitize n
                        , Elm.list []
                            |> Elm.withType
                                (Gen.Svg.Styled.annotation_.attribute
                                    (Annotation.var "msg")
                                    |> Annotation.list
                                )
                        )
                    )
                >> Elm.record

        funName =
            sanitize metadata.name

        attributesParamName =
            "childrenAttributes"

        propertiesParamName =
            "properties"

        attributesParam =
            ( attributesParamName
            , names
                |> List.map sanitize
                |> Set.fromList
                |> Set.toList
                |> List.map (\n -> ( n, Gen.Svg.Styled.annotation_.attribute (Annotation.var "msg") |> Annotation.list ))
                |> Annotation.record
                |> Just
            )

        propertiesParam =
            ( propertiesParamName
            , properties |> propertiesType |> Just
            )
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
                |> Elm.declaration ("default " ++ metadata.name ++ " attributes" |> sanitize)
            , Elm.fn2
                attributesParam
                propertiesParam
                (\attributes properties_ ->
                    let
                        config =
                            { propertyExpressions =
                                node.componentPropertiesTrait.componentPropertyDefinitions
                                    |> Maybe.map
                                        (Dict.map (\nam _ -> properties_ |> Elm.get (formatName nam)))
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
                |> Elm.declaration funName
            , Elm.fn3
                ( "attributes", Nothing )
                attributesParam
                propertiesParam
                (\attributes_ childrenAttributes properties_ ->
                    Elm.apply
                        (Elm.value
                            { importFrom = []
                            , name = funName
                            , annotation = Nothing
                            }
                        )
                        [ childrenAttributes
                        , properties_
                        ]
                        |> List.singleton
                        |> Elm.list
                        |> Gen.Svg.Styled.call_.svg
                            (attributes_
                                |> Elm.Op.append
                                    ([ Attributes.width <| String.fromFloat metadata.bbox.width
                                     , Attributes.height <| String.fromFloat metadata.bbox.height
                                     ]
                                        |> Elm.list
                                    )
                            )
                )
                |> Elm.declaration (sanitize <| funName ++ " svg")
            ]


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
                |> Attributes.transform
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

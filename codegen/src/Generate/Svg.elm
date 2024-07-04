module Generate.Svg exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (uncurry)
import Dict
import Elm
import Elm.Annotation as Annotation
import Elm.Op
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes as Attributes
import Generate.Common exposing (adjustBoundingBoxes)
import Generate.Common.FrameTraits
import Generate.Svg.DefaultShapeTraits as DefaultShapeTraits
import Generate.Svg.EllipseNode as EllipseNode
import Generate.Svg.FrameTraits as FrameTraits
import Generate.Svg.RectangleNode as RectangleNode
import Generate.Svg.TextNode as TextNode
import Generate.Svg.VectorNode as VectorNode
import Generate.Util exposing (detailsToDeclaration, getElementAttributes, sanitize, toTranslate, withVisibility)
import Set
import String.Case exposing (toCamelCaseUpper)
import String.Extra
import Tuple exposing (mapFirst)
import Types exposing (Config, Details)


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

        SubcanvasNodeRectangleNode n ->
            RectangleNode.toDetails n
                |> List.singleton

        SubcanvasNodeVectorNode n ->
            VectorNode.toDetails n
                |> List.singleton

        _ ->
            []


componentNodeToDeclarations : ComponentNode -> List Elm.Declaration
componentNodeToDeclarations node =
    let
        ( details, descendantsDetails ) =
            withFrameTraitsNodeToDetails node

        names =
            details.name
                :: List.map .name descendantsDetails

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
            sanitize details.name

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
    details
        :: descendantsDetails
        |> List.foldl
            (\md ->
                Dict.insert md.name md
            )
            Dict.empty
        |> Dict.values
        |> List.map (detailsToDeclaration details.name)
        |> (++)
            [ properties
                |> propertiesType
                |> Elm.alias (details.name ++ " properties" |> toCamelCaseUpper)
            , names
                |> defaultAttributeConfig
                |> Elm.declaration ("default " ++ details.name ++ " attributes" |> sanitize)
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
                        (getElementAttributes config details.name)
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
                                    ([ Attributes.width <| String.fromFloat details.bbox.width
                                     , Attributes.height <| String.fromFloat details.bbox.height
                                     ]
                                        |> Elm.list
                                    )
                            )
                )
                |> Elm.declaration (sanitize <| funName ++ " svg")
            ]


withFrameTraitsNodeToDetails : { a | frameTraits : FrameTraits } -> ( Details, List Details )
withFrameTraitsNodeToDetails node =
    ( FrameTraits.toDetails node
    , node.frameTraits.children
        |> List.map subcanvasNodeToDetails
        |> List.concat
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


frameTraitsToExpressions : Config -> FrameTraits -> List Elm.Expression
frameTraitsToExpressions config node =
    node.children
        |> List.map (subcanvasNodeToExpressions config)
        |> List.concat


withFrameTraitsNodeToExpressions : Config -> { a | frameTraits : FrameTraits } -> List Elm.Expression
withFrameTraitsNodeToExpressions config node =
    let
        { name } =
            Generate.Common.FrameTraits.toDetails FrameTraits.toStyles node
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

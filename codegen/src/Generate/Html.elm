module Generate.Html exposing (..)

{-| -}

import Api.Raw exposing (..)
import Basics.Extra exposing (uncurry)
import Dict exposing (Dict)
import Elm
import Elm.Annotation as Annotation
import Elm.Case
import Elm.Op
import Gen.Css as Css
import Gen.Html.Styled
import Gen.Html.Styled.Attributes as Attributes
import Gen.Maybe
import Generate.Common as Common
import Generate.Common.FrameTraits
import Generate.Html.ComponentNode as ComponentNode
import Generate.Html.DefaultShapeTraits as DefaultShapeTraits
import Generate.Html.FrameTraits as FrameTraits
import Generate.Html.LineNode as LineNode
import Generate.Html.RectangleNode as RectangleNode
import Generate.Html.TextNode as TextNode
import Generate.Html.VectorNode as VectorNode
import Generate.Util exposing (detailsToDeclaration, getByNameId, getElementAttributes, sanitize, withVisibility)
import Maybe.Extra
import RecordSetter exposing (s_styles)
import Set
import String.Case exposing (toCamelCaseUpper)
import String.Extra
import Tuple exposing (mapBoth, pair)
import Types exposing (Config, Details)


subcanvasNodeComponentsToDeclarations : String -> SubcanvasNode -> List Elm.Declaration
subcanvasNodeComponentsToDeclarations parentName node =
    case node of
        SubcanvasNodeComponentNode n ->
            Common.adjustBoundingBoxes n
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

        SubcanvasNodeGroupNode n ->
            withFrameTraitsNodeToExpressions config name n

        SubcanvasNodeFrameNode n ->
            withFrameTraitsNodeToExpressions config name n

        SubcanvasNodeInstanceNode n ->
            instanceNodeToExpressions config name n

        SubcanvasNodeRectangleNode n ->
            RectangleNode.toExpressions config name n

        SubcanvasNodeVectorNode n ->
            VectorNode.toExpressions config name n

        SubcanvasNodeLineNode n ->
            DefaultShapeTraits.toExpressions config name n

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
            , Elm.fn3
                attributesParam
                instancesParam
                propertiesParam
                (\attributes instances properties_ ->
                    let
                        config =
                            { propertyExpressions =
                                Common.propertiesToPropertyExpressions properties_ properties
                                    |> Debug.log ("abc properties of " ++ details.name)
                            , attributes = attributes
                            , instances = instances
                            }
                    in
                    Gen.Html.Styled.call_.div
                        (getElementAttributes config details.name
                            |> Elm.Op.append
                                (ComponentNode.toStyles node
                                    |> Attributes.css
                                    |> List.singleton
                                    |> Elm.list
                                )
                        )
                        (frameTraitsToExpressions config details.name node.frameTraits
                            |> Elm.list
                        )
                )
                |> Elm.withType
                    (Annotation.function
                        [ attributesType, instancesType, propertiesType ]
                        (Gen.Html.Styled.annotation_.html (Annotation.var "msg"))
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
                        (Gen.Html.Styled.annotation_.html (Annotation.var "msg"))
                    )
                |> Elm.declaration (sanitize declarationName)
            , Elm.fn2
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
            ]


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
                |> Debug.log "123 withFrameTraitsToExp name"
    in
    Gen.Html.Styled.call_.div
        (getElementAttributes config name
            |> Elm.Op.append
                (FrameTraits.toStyles node.frameTraits
                    |> Attributes.css
                    |> List.singleton
                    |> Elm.list
                )
        )
        (frameTraitsToExpressions config componentName node.frameTraits
            |> Elm.list
        )
        |> withVisibility (Debug.log "123 withFrameTraitsToExp withVisibility" componentName) (Debug.log "123 propertyExpressions" config.propertyExpressions) node.frameTraits.isLayerTrait.componentPropertyReferences
        |> List.singleton


instanceNodeToExpressions : Config -> String -> InstanceNode -> List Elm.Expression
instanceNodeToExpressions config parentName node =
    let
        name =
            Generate.Common.FrameTraits.getName node
                |> Debug.log "123 instanceNodeToExp name"

        subNameId =
            Debug.log "123 instanceNodeToExp subNameId" <|
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
                    (withVisibility parentName config.propertyExpressions node.frameTraits.isLayerTrait.componentPropertyReferences)
                |> Maybe.Extra.withDefaultLazy
                    (\_ ->
                        Gen.Html.Styled.call_.div
                            (getElementAttributes config name
                                |> Elm.Op.append
                                    (FrameTraits.toStyles node.frameTraits
                                        |> Attributes.css
                                        |> List.singleton
                                        |> Elm.list
                                    )
                            )
                            (frameTraitsToExpressions config subNameId node.frameTraits
                                |> Elm.list
                            )
                            |> withVisibility (Debug.log "123 instanceNodeToExp withVisibility" parentName) (Debug.log "123 propertyExpressions" config.propertyExpressions) node.frameTraits.isLayerTrait.componentPropertyReferences
                    )
            )
        |> List.singleton


instanceNodeToStyles : InstanceNode -> List Elm.Expression
instanceNodeToStyles _ =
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

        SubcanvasNodeLineNode n ->
            LineNode.toDetails n
                |> List.singleton

        _ ->
            []


componentNodeToDetails : ComponentNode -> ( Details, List Details )
componentNodeToDetails node =
    ( FrameTraits.toDetails node
        |> s_styles (ComponentNode.toStyles node)
    , node.frameTraits.children
        |> List.map subcanvasNodeToDetails
        |> List.concat
    )


withFrameTraitsNodeToDetails : { a | frameTraits : FrameTraits } -> ( Details, List Details )
withFrameTraitsNodeToDetails node =
    ( FrameTraits.toDetails node
    , node.frameTraits.children
        |> List.map subcanvasNodeToDetails
        |> List.concat
    )

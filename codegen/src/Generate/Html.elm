module Generate.Html exposing (..)

{-| -}

import Api.Raw exposing (..)
import Basics.Extra exposing (uncurry)
import Dict
import Elm
import Elm.Annotation as Annotation
import Elm.Op
import Gen.Html.Styled
import Gen.Html.Styled.Attributes as Attributes
import Generate.Common.FrameTraits
import Generate.Html.ComponentNode as ComponentNode
import Generate.Html.DefaultShapeTraits as DefaultShapeTraits
import Generate.Html.FrameTraits as FrameTraits
import Generate.Html.RectangleNode as RectangleNode
import Generate.Html.TextNode as TextNode
import Generate.Html.VectorNode as VectorNode
import Generate.Util exposing (detailsToDeclaration, getElementAttributes, sanitize, withVisibility)
import Set
import String.Case exposing (toCamelCaseUpper)
import String.Extra
import Tuple exposing (mapFirst)
import Types exposing (Config, Details)


subcanvasNodeComponentsToDeclarations : SubcanvasNode -> List Elm.Declaration
subcanvasNodeComponentsToDeclarations node =
    case node of
        SubcanvasNodeComponentNode n ->
            componentNodeToDeclarations n

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

        SubcanvasNodeGroupNode n ->
            withFrameTraitsNodeToExpressions config n

        SubcanvasNodeFrameNode n ->
            withFrameTraitsNodeToExpressions config n

        SubcanvasNodeInstanceNode n ->
            instanceNodeToExpressions config n

        SubcanvasNodeRectangleNode n ->
            RectangleNode.toExpressions config n

        SubcanvasNodeVectorNode n ->
            VectorNode.toExpressions config n

        _ ->
            []


subcanvasNodeToStyles : SubcanvasNode -> List Elm.Expression
subcanvasNodeToStyles node =
    case node of
        SubcanvasNodeTextNode n ->
            TextNode.toStyles n

        SubcanvasNodeGroupNode n ->
            FrameTraits.toStyles n.frameTraits

        SubcanvasNodeFrameNode n ->
            FrameTraits.toStyles n.frameTraits

        SubcanvasNodeInstanceNode n ->
            instanceNodeToStyles n

        SubcanvasNodeRectangleNode n ->
            RectangleNode.toStyles n

        SubcanvasNodeVectorNode _ ->
            []

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
                                (Gen.Html.Styled.annotation_.attribute
                                    (Annotation.var "msg")
                                    |> Annotation.list
                                )
                        )
                    )
                >> Elm.record
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
                ( "attributes"
                , names
                    |> List.map (\n -> ( n, Gen.Html.Styled.annotation_.attribute (Annotation.var "msg") |> Annotation.list ))
                    |> Annotation.record
                    |> Just
                )
                ( "properties"
                , Nothing
                )
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
                    Gen.Html.Styled.call_.div
                        (getElementAttributes config details.name
                            |> Elm.Op.append
                                (ComponentNode.toStyles node
                                    |> Attributes.css
                                    |> List.singleton
                                    |> Elm.list
                                )
                        )
                        (frameTraitsToExpressions config node.frameTraits
                            |> Elm.list
                        )
                )
                |> Elm.declaration (sanitize details.name)
            ]


frameTraitsToExpressions : Config -> FrameTraits -> List Elm.Expression
frameTraitsToExpressions config node =
    node.children
        |> List.map (subcanvasNodeToExpressions config)
        |> List.concat


propertiesType : Dict.Dict String ComponentPropertyDefinition -> Annotation.Annotation
propertiesType =
    Dict.map
        (\_ def ->
            case def.type_ of
                ComponentPropertyTypeBOOLEAN ->
                    Annotation.bool

                ComponentPropertyTypeINSTANCESWAP ->
                    Gen.Html.Styled.annotation_.html (Annotation.var "msg")

                ComponentPropertyTypeTEXT ->
                    Annotation.string

                ComponentPropertyTypeVARIANT ->
                    Debug.todo "support variant"
        )
        >> Dict.toList
        >> Annotation.record


withFrameTraitsNodeToExpressions : Config -> { a | frameTraits : FrameTraits } -> List Elm.Expression
withFrameTraitsNodeToExpressions config node =
    let
        name =
            Generate.Common.FrameTraits.getName node
    in
    Gen.Html.Styled.call_.div
        (getElementAttributes config name)
        (frameTraitsToExpressions config node.frameTraits
            |> Elm.list
        )
        |> withVisibility config.propertyExpressions node.frameTraits.isLayerTrait.componentPropertyReferences
        |> List.singleton


instanceNodeToExpressions : Config -> InstanceNode -> List Elm.Expression
instanceNodeToExpressions config node =
    node.frameTraits.isLayerTrait.componentPropertyReferences
        |> Maybe.andThen (Dict.get "mainComponent")
        |> Maybe.andThen (\ref -> Dict.get ref config.propertyExpressions)
        |> Maybe.map
            (List.singleton
                >> Gen.Html.Styled.div []
                >> List.singleton
            )
        |> Maybe.withDefault
            (withFrameTraitsNodeToExpressions config node)


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

        _ ->
            []


withFrameTraitsNodeToDetails : { a | frameTraits : FrameTraits } -> ( Details, List Details )
withFrameTraitsNodeToDetails node =
    ( FrameTraits.toDetails node
    , node.frameTraits.children
        |> List.map subcanvasNodeToDetails
        |> List.concat
    )

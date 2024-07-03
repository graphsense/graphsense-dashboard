module Generate.Html exposing (..)

{-| -}

import Api.Raw exposing (..)
import Dict
import Elm
import Elm.Annotation as Annotation
import Elm.Op
import Gen.Html.Styled
import Gen.Html.Styled.Attributes as Attributes
import Generate.Common as Common
import Generate.Common.FrameTraits as FrameTraits
import Generate.Html.ComponentNode as ComponentNode
import Generate.Html.RectangleNode as RectangleNode
import Generate.Html.TextNode as TextNode
import Generate.Util exposing (getElementAttributes, metadataToDeclaration, withVisibility)
import Set
import String.Case exposing (toCamelCaseLower, toCamelCaseUpper)
import String.Extra
import Tuple exposing (mapFirst)
import Types exposing (Config)


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

        defaultAttributeConfig : List String -> Elm.Expression
        defaultAttributeConfig =
            Set.fromList
                >> Set.toList
                >> List.map
                    (\n ->
                        ( toCamelCaseLower n
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
                        (getElementAttributes config metadata.name
                            |> Elm.Op.append
                                (ComponentNode.toCss node
                                    |> Attributes.css
                                    |> List.singleton
                                    |> Elm.list
                                )
                        )
                        (frameTraitsToExpressions config node.frameTraits
                            |> Elm.list
                        )
                )
                |> Elm.declaration (toCamelCaseLower metadata.name)
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
        { name } =
            FrameTraits.toMetadata node
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

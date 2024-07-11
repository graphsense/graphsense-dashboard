module Generate.Html exposing (..)

{-| -}

import Api.Raw exposing (..)
import Basics.Extra exposing (uncurry)
import Dict exposing (Dict)
import Elm
import Elm.Annotation as Annotation
import Elm.Op
import Gen.Css as Css
import Gen.Html.Styled
import Gen.Html.Styled.Attributes as Attributes
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


subcanvasNodeComponentsToDeclarations : SubcanvasNode -> List Elm.Declaration
subcanvasNodeComponentsToDeclarations node =
    case node of
        SubcanvasNodeComponentNode n ->
            Common.adjustBoundingBoxes n
                |> componentNodeToDeclarations

        SubcanvasNodeComponentSetNode n ->
            n.frameTraits.children
                |> List.map subcanvasNodeComponentsToDeclarations
                |> List.concat

        _ ->
            []


subcanvasNodeToExpressions : Config -> ( String, String ) -> SubcanvasNode -> List Elm.Expression
subcanvasNodeToExpressions config nameId node =
    case node of
        SubcanvasNodeTextNode n ->
            TextNode.toExpressions config nameId n

        SubcanvasNodeGroupNode n ->
            withFrameTraitsNodeToExpressions config nameId n

        SubcanvasNodeFrameNode n ->
            withFrameTraitsNodeToExpressions config nameId n

        SubcanvasNodeInstanceNode n ->
            instanceNodeToExpressions config nameId n

        SubcanvasNodeRectangleNode n ->
            RectangleNode.toExpressions config nameId n

        SubcanvasNodeVectorNode n ->
            VectorNode.toExpressions config nameId n

        SubcanvasNodeLineNode n ->
            DefaultShapeTraits.toExpressions config nameId n

        _ ->
            []


componentNodeToDeclarations : ComponentNode -> List Elm.Declaration
componentNodeToDeclarations node =
    let
        ( details, descendantsDetails ) =
            componentNodeToDetails node

        names =
            details.name
                :: List.map .name descendantsDetails

        properties =
            Common.componentNodeToProperties details.name node

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

        nameId =
            ( details.name, Generate.Common.FrameTraits.getId node )

        propertiesType =
            properties
                |> Common.propertiesType Gen.Html.Styled.annotation_.html
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
            [ propertiesType
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
                , Just propertiesType
                )
                (\attributes properties_ ->
                    let
                        config =
                            { propertyExpressions =
                                Common.propertiesToPropertyExpressions properties_ properties
                                    |> Debug.log ("abc properties of " ++ details.name)
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
                        (frameTraitsToExpressions config nameId node.frameTraits
                            |> Elm.list
                        )
                )
                |> Elm.declaration (sanitize details.name)
            ]


frameTraitsToExpressions : Config -> ( String, String ) -> FrameTraits -> List Elm.Expression
frameTraitsToExpressions config nameId node =
    node.children
        |> List.map (subcanvasNodeToExpressions config nameId)
        |> List.concat


withFrameTraitsNodeToExpressions : Config -> ( String, String ) -> { a | frameTraits : FrameTraits } -> List Elm.Expression
withFrameTraitsNodeToExpressions config nameId node =
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
        (frameTraitsToExpressions config nameId node.frameTraits
            |> Elm.list
        )
        |> withVisibility (Debug.log "123 withFrameTraitsToExp withVisibility" nameId) (Debug.log "123 propertyExpressions" config.propertyExpressions) node.frameTraits.isLayerTrait.componentPropertyReferences
        |> List.singleton


instanceNodeToExpressions : Config -> ( String, String ) -> InstanceNode -> List Elm.Expression
instanceNodeToExpressions config parentNameId node =
    let
        name =
            Generate.Common.FrameTraits.getName node
                |> Debug.log "123 instanceNodeToExp name"

        id =
            Generate.Common.FrameTraits.getId node
                |> Debug.log "123 instanceNodeToExp id"

        _ =
            Debug.log "123 instanceNodeToExp parentNameId" parentNameId

        subNameId =
            Debug.log "123 instanceNodeToExp subNameId" <|
                if node.componentProperties /= Nothing then
                    ( name, id )

                else
                    parentNameId
    in
    node.frameTraits.isLayerTrait.componentPropertyReferences
        |> Maybe.andThen (Dict.get "mainComponent")
        |> Maybe.andThen
            (\ref ->
                getByNameId parentNameId config.propertyExpressions
                    |> Maybe.andThen (Dict.get ref)
            )
        |> Maybe.map
            (withVisibility parentNameId config.propertyExpressions node.frameTraits.isLayerTrait.componentPropertyReferences
                >> List.singleton
            )
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
                    |> withVisibility (Debug.log "123 instanceNodeToExp withVisibility" parentNameId) (Debug.log "123 propertyExpressions" config.propertyExpressions) node.frameTraits.isLayerTrait.componentPropertyReferences
                    |> List.singleton
            )


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

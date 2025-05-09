module Generate.Svg exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (uncurry)
import Dict
import Elm
import Gen.Maybe
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes as Attributes
import Generate.Common exposing (hasMainComponentProperty, hasVariantProperty)
import Generate.Common.DefaultShapeTraits
import Generate.Common.FrameTraits
import Generate.Svg.EllipseNode as EllipseNode
import Generate.Svg.FrameTraits as FrameTraits
import Generate.Svg.LineNode as LineNode
import Generate.Svg.RectangleNode as RectangleNode
import Generate.Svg.TextNode as TextNode
import Generate.Svg.VectorNode as VectorNode
import Generate.Util exposing (getElementAttributes, sanitize, toTranslate, withVisibility)
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

        SubcanvasNodeEllipseNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n then
                []

            else
                EllipseNode.toExpressions config name n

        SubcanvasNodeGroupNode n ->
            if Generate.Common.FrameTraits.isHidden n then
                []

            else
                withFrameTraitsNodeToExpressions config name n

        SubcanvasNodeFrameNode n ->
            if Generate.Common.FrameTraits.isHidden n then
                []

            else
                withFrameTraitsNodeToExpressions config name n

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

        SubcanvasNodeVectorNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n.cornerRadiusShapeTraits then
                []

            else
                VectorNode.toExpressions config name n.cornerRadiusShapeTraits

        SubcanvasNodeRectangleNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n.rectangularShapeTraits then
                []

            else
                RectangleNode.toExpressions config name n

        SubcanvasNodeLineNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n then
                []

            else
                LineNode.toExpressions config name n

        _ ->
            []


subcanvasNodeToStyles : ColorMap -> SubcanvasNode -> List Styles
subcanvasNodeToStyles colorMap node =
    case node of
        SubcanvasNodeComponentNode n ->
            if Generate.Common.FrameTraits.isHidden n then
                []

            else
                withFrameTraitsNodeToStyles colorMap n
                    |> uncurry (::)

        SubcanvasNodeComponentSetNode n ->
            if Generate.Common.FrameTraits.isHidden n then
                []

            else
                withFrameTraitsNodeToStyles colorMap n
                    |> uncurry (::)

        SubcanvasNodeTextNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n then
                []

            else
                TextNode.toStyles colorMap n
                    |> List.singleton

        SubcanvasNodeEllipseNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n then
                []

            else
                EllipseNode.toStyles colorMap n
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
            if hasVariantProperty n || hasMainComponentProperty n then
                []

            else
                withFrameTraitsNodeToStyles colorMap n
                    |> uncurry (::)

        SubcanvasNodeRectangleNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n.rectangularShapeTraits then
                []

            else
                RectangleNode.toStyles colorMap n
                    |> List.singleton

        SubcanvasNodeVectorNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n.cornerRadiusShapeTraits then
                []

            else
                VectorNode.toStyles colorMap n.cornerRadiusShapeTraits
                    |> List.singleton

        SubcanvasNodeLineNode n ->
            if Generate.Common.DefaultShapeTraits.isHidden n then
                []

            else
                LineNode.toStyles colorMap n
                    |> List.singleton

        _ ->
            []



{-
   componentNodeToDeclarations : ColorMap -> String -> Dict String (Dict String ComponentPropertyType) -> ComponentNodeOrSet -> List Elm.Declaration
   componentNodeToDeclarations colorMap parentName parentProperties node =
       let
           ( details, descendantsDetails ) =
               Common.componentNodeToDetails node

           ( styles, descendantsStyles ) =
               componentNodeToStyles colorMap node

           names =
               rootName
                   :: List.map .name descendantsDetails

           properties =
               Common.componentNodeToProperties details.name node
                   |> Dict.union parentProperties

           propertiesType =
               properties
                   |> Common.propertiesType details.name Gen.Svg.Styled.annotation_.svg

           declarationName =
               parentName
                   ++ " "
                   ++ details.name
                   |> Debug.log "DEBUG declarationName"
                   |> sanitize
                   |> Debug.log "DEBUG declarationName after"

           propertiesParamName =
               "properties"

           propertiesParam =
               ( propertiesParamName
               , propertiesType |> Just
               )

           attributesParam =
               ( "attributes", Nothing )

           attributesType =
               Gen.Svg.Styled.annotation_.attribute (Annotation.var "msg") |> Annotation.list

           instancesType =
               names
                   |> List.map
                       (\n ->
                           ( n
                           , Gen.Svg.Styled.annotation_.svg (Annotation.var "msg")
                               |> Annotation.maybe
                           )
                       )
                   |> Annotation.extensible "i"

           instancesParam =
               ( "instances"
               , instancesType
                   |> Just
               )

           declarationName =
               parentName
                   ++ " "
                   ++ details.name
                   |> Debug.log "DEBUG declarationName"
                   |> sanitize
                   |> Debug.log "DEBUG declarationName after"

           declarationNameWithInstances =
               declarationName
                   ++ " with instances"
                   |> sanitize
                   |> Debug.log "DEBUG declarationNameWithI"

           declarationNameWithAttributes =
               declarationName ++ " with attributes" |> sanitize

           declarationNameWithInstancesSvg =
               declarationNameWithInstances ++ " svg" |> sanitize

           declarationNameAttributes =
               declarationName ++ " attributes" |> sanitize

           declarationNameInstances =
               declarationName ++ " instances" |> sanitize
       in
       (styles :: descendantsStyles)
           |> List.map2 pair
               (details
                   :: descendantsDetails
               )
           |> List.foldl
               (\md ->
                   Dict.insert (first md).name md
               )
               Dict.empty
           |> Dict.values
           |> List.map (detailsAndStylesToDeclaration parentName details.name)
           |> (++)
               [ properties
                   |> Common.propertiesType details.name Gen.Svg.Styled.annotation_.svg
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
                               , children = Elm.record []
                               , instances = instances
                               , colorMap = colorMap
                               , parentName = parentName
                               , componentName = details.name
                               , instanceName = ""
                               , showId = showId
                               }
                       in
                       withFrameTraitsNodeToExpression config details.name "" node
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
-}


withFrameTraitsNodeToExpression : Config -> String -> String -> FrameTraits -> Elm.Expression
withFrameTraitsNodeToExpression config componentName componentNameForChildren node =
    Gen.Svg.Styled.call_.g
        (getElementAttributes config componentName)
        (frameTraitsToExpressions config componentNameForChildren node
            |> Elm.list
        )


withFrameTraitsNodeToStyles : ColorMap -> { a | frameTraits : FrameTraits } -> ( Styles, List Styles )
withFrameTraitsNodeToStyles colorMap node =
    ( FrameTraits.toStyles node.frameTraits
    , node.frameTraits.children
        |> List.map (subcanvasNodeToStyles colorMap)
        |> List.concat
    )


componentNodeToStyles : ColorMap -> FrameTraits -> ( Styles, List Styles )
componentNodeToStyles colorMap frameTraits =
    withFrameTraitsNodeToStyles colorMap { frameTraits = frameTraits }


frameTraitsToExpressions : Config -> String -> FrameTraits -> List Elm.Expression
frameTraitsToExpressions config componentName node =
    if Generate.Common.FrameTraits.isList { frameTraits = node } then
        -- maybe add config.children here later like in Html
        []

    else
        node.children
            |> List.map (subcanvasNodeToExpressions config componentName)
            |> List.concat


withFrameTraitsNodeToExpressions : Config -> String -> { a | frameTraits : FrameTraits } -> List Elm.Expression
withFrameTraitsNodeToExpressions config componentName node =
    let
        name =
            Generate.Common.FrameTraits.getName node.frameTraits
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
            |> Maybe.map
                (List.singleton
                    >> Elm.list
                    >> Gen.Svg.Styled.call_.g (Elm.list [ coords ])
                )
            |> Maybe.Extra.withDefaultLazy
                (\_ ->
                    Elm.get name config.instances
                        |> Gen.Maybe.withDefault
                            (Gen.Svg.Styled.call_.g
                                (getElementAttributes config name)
                                (frameTraitsToExpressions config subNameId node.frameTraits
                                    |> Elm.list
                                )
                            )
                )
        )
            |> withVisibility parentName config.propertyExpressions node.frameTraits.isLayerTrait.componentPropertyReferences
            |> List.singleton

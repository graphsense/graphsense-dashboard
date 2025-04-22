module Generate.Common exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (flip, uncurry)
import Config exposing (showId)
import Dict exposing (Dict)
import Dict.Extra
import Elm exposing (Expression)
import Elm.Annotation as Annotation exposing (Annotation)
import Elm.Let
import Elm.Op
import Gen.Css as Css
import Gen.Html as Html
import Gen.Html.Styled
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes
import Generate.Common.DefaultShapeTraits as DefaultShapeTraits
import Generate.Common.FrameTraits as FrameTraits
import Generate.Common.RectangleNode as RectangleNode
import Generate.Common.VectorNode as VectorNode
import Generate.Html.HasLayoutTrait as HasLayoutTrait
import Generate.Util exposing (detailsAndStylesToDeclaration, getElementAttributes, getMainComponentProperty, mm2, rootName, sanitize)
import List.Extra
import RecordSetter exposing (s_children, s_defaultShapeTraits, s_frameTraits, s_instanceName, s_name)
import Set
import String.Case exposing (toCamelCaseUpper)
import String.Extra
import Tuple exposing (first, mapFirst, mapSecond, pair, second)
import Types exposing (ColorMap, ComponentNodeOrSet, ComponentPropertyExpressions, Config, Details, FormatSpecifics, OriginAdjust)


subcanvasNodeComponentsToDeclarations : FormatSpecifics -> ColorMap -> SubcanvasNode -> List Elm.Declaration
subcanvasNodeComponentsToDeclarations specifics colorMap node =
    let
        filter n =
            filterUnneededParts n
                |> adjustBoundingBoxes
    in
    case node of
        SubcanvasNodeComponentNode n ->
            if FrameTraits.isHidden n then
                []

            else
                filter n
                    |> adjustNames
                    |> toComponentNodeOrSet
                    |> componentNodeToDeclarations specifics colorMap "" Dict.empty

        SubcanvasNodeComponentSetNode n ->
            if FrameTraits.isHidden n then
                []

            else
                filter n
                    |> toComponentNodeOrSet
                    |> componentSetNodeToDeclarations specifics colorMap

        _ ->
            []


toComponentNodeOrSet : { a | componentPropertiesTrait : ComponentPropertiesTrait, frameTraits : FrameTraits } -> ComponentNodeOrSet
toComponentNodeOrSet n =
    { componentPropertiesTrait = n.componentPropertiesTrait
    , frameTraits = n.frameTraits
    }


componentNodesForComponentSet : FormatSpecifics -> ColorMap -> Dict String (Dict String ComponentPropertyType) -> ComponentNodeOrSet -> String -> List Elm.Declaration
componentNodesForComponentSet formatSpecifics colorMap parentProperties n parentName =
    n.frameTraits.children
        |> List.map
            (\child ->
                case child of
                    SubcanvasNodeComponentNode nn ->
                        toComponentNodeOrSet nn
                            |> componentNodeToDeclarations
                                formatSpecifics
                                colorMap
                                parentName
                                parentProperties

                    _ ->
                        []
            )
        |> List.concat


componentSetNodeToDeclarations : FormatSpecifics -> ColorMap -> ComponentNodeOrSet -> List Elm.Declaration
componentSetNodeToDeclarations ({ elementAnnotation, attributeAnnotation } as formatSpecifics) colorMap node =
    let
        sanitizedChildrenNode =
            node.frameTraits.children
                |> List.filterMap
                    (\child ->
                        case child of
                            SubcanvasNodeComponentNode nn ->
                                adjustNames nn
                                    |> SubcanvasNodeComponentNode
                                    |> Just

                            _ ->
                                Nothing
                    )
                |> flip s_children node.frameTraits
                |> flip s_frameTraits node

        ( details, descendantsDetails ) =
            componentNodeToDetails sanitizedChildrenNode

        names =
            rootName
                :: List.map .name descendantsDetails
                |> Set.fromList
                |> Set.toList

        attributesType =
            names
                |> List.map sanitize
                |> List.map (\n -> ( n, attributeAnnotation (Annotation.var "msg") |> Annotation.list ))
                |> Annotation.record

        attributesParam =
            ( "attributes"
            , attributesType
                |> Just
            )

        instancesType =
            names
                |> List.map sanitize
                |> List.map
                    (\n ->
                        ( n
                        , elementAnnotation (Annotation.var "msg")
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
            details.name
                :: List.map .name descendantsDetails
                |> List.filter FrameTraits.nameIsList

        childrenType =
            namesWithList
                |> List.map
                    (\n ->
                        ( n
                        , elementAnnotation (Annotation.var "msg")
                            |> Annotation.list
                        )
                    )
                |> Annotation.record

        childrenParam =
            ( "children"
            , childrenType
                |> Just
            )

        name =
            FrameTraits.getName node.frameTraits
                |> sanitize

        properties =
            sanitizedChildrenNode
                |> componentNodeToProperties name

        nothingIfEmpty d =
            if Dict.isEmpty d then
                Nothing

            else
                Just d

        propertiesWithoutVariants =
            properties
                |> Dict.toList
                |> List.filterMap
                    (\( k, types ) ->
                        if k == rootName then
                            Dict.filter
                                (\_ type_ ->
                                    type_ /= ComponentPropertyTypeVARIANT
                                )
                                types
                                |> nothingIfEmpty
                                |> Maybe.map (pair (sanitize k))

                        else
                            Just ( sanitize k, types )
                    )
                |> Dict.fromList

        variantProperties : List ( String, List String )
        variantProperties =
            node.componentPropertiesTrait.componentPropertyDefinitions
                |> Maybe.withDefault Dict.empty
                |> Dict.toList
                |> List.filter (second >> .type_ >> (==) ComponentPropertyTypeVARIANT)
                |> List.filterMap
                    (\( nam, def ) ->
                        def.variantOptions
                            |> Maybe.map (pair nam)
                    )

        propertiesType_ =
            properties
                |> propertiesType name elementAnnotation

        propertiesParam =
            ( "properties"
            , Just propertiesType_
            )

        declarationName =
            name |> sanitize

        declarationNameAttributes =
            name ++ " attributes" |> sanitize

        declarationNameInstances =
            name ++ " instances" |> sanitize

        declarationNameWithInstances =
            (name ++ " with instances") |> sanitize

        declarationNameWithAttributes =
            (name ++ " with attributes") |> sanitize

        componentFunctionWithChildren attributesRecord instancesRecord childrenRecord propertiesRecord nam =
            Elm.apply
                (Elm.value
                    { importFrom = []
                    , name = nam
                    , annotation = Nothing
                    }
                )
                [ attributesRecord
                , instancesRecord
                , childrenRecord
                , propertiesRecord
                ]

        componentFunction attributesRecord instancesRecord propertiesRecord nam =
            Elm.apply
                (Elm.value
                    { importFrom = []
                    , name = nam
                    , annotation = Nothing
                    }
                )
                [ attributesRecord
                , instancesRecord
                , propertiesRecord
                ]

        propsWithoutVariantsForLetIn properties_ =
            propertiesWithoutVariants
                |> Dict.toList
                |> List.map
                    (\( comp, props ) ->
                        props
                            |> Dict.keys
                            |> List.map
                                (\k ->
                                    let
                                        kk =
                                            formatComponentPropertyName k
                                                |> sanitize
                                    in
                                    ( kk, Elm.get comp properties_ |> Elm.get kk )
                                )
                            |> Elm.record
                            |> pair comp
                    )
                |> Elm.record

        body properties_ compFunction =
            -- using the non-name-adjusted children nodes here
            node.frameTraits.children
                |> List.filterMap
                    (\child ->
                        case child of
                            SubcanvasNodeComponentNode n ->
                                Just n

                            _ ->
                                Nothing
                    )
                |> List.Extra.uncons
                |> Maybe.map
                    (\( fst, rest ) ->
                        let
                            nam w =
                                name
                                    ++ " "
                                    ++ FrameTraits.getName w.frameTraits
                                    ++ " with instances"
                                    |> sanitize
                        in
                        List.foldl
                            (\comp acc ->
                                Elm.ifThen
                                    (FrameTraits.getName comp.frameTraits
                                        |> variantNameToComparisionExpression properties_ name
                                    )
                                    (nam comp
                                        |> compFunction
                                    )
                                    acc
                            )
                            (nam fst |> compFunction)
                            rest
                    )
                |> Maybe.withDefault (Html.text "")
    in
    (propertiesType_
        |> Elm.alias (name ++ " properties" |> sanitize)
    )
        :: (variantProperties |> List.map (uncurry (variantPropertyToType name)))
        ++ [ names
                |> defaultAttributeConfig attributeAnnotation
                |> Elm.declaration declarationNameAttributes
           , names
                |> defaultInstancesConfig elementAnnotation
                |> Elm.declaration declarationNameInstances
           , if List.isEmpty namesWithList then
                Elm.fn3 attributesParam
                    instancesParam
                    propertiesParam
                    (\attributes instances properties_ ->
                        Elm.Let.letIn
                            (componentFunction attributes instances
                                >> body properties_
                            )
                            |> Elm.Let.value "propsWithoutVariants" (propsWithoutVariantsForLetIn properties_)
                            |> Elm.Let.toExpression
                    )
                    |> Elm.withType
                        (Annotation.function
                            [ attributesType, instancesType, propertiesType_ ]
                            (elementAnnotation (Annotation.var "msg"))
                        )
                    |> Elm.declaration declarationNameWithInstances

             else
                Elm.fn4 attributesParam
                    instancesParam
                    childrenParam
                    propertiesParam
                    (\attributes instances children properties_ ->
                        Elm.Let.letIn
                            (componentFunctionWithChildren attributes instances children
                                >> body properties_
                            )
                            |> Elm.Let.value "propsWithoutVariants" (propsWithoutVariantsForLetIn properties_)
                            |> Elm.Let.toExpression
                    )
                    |> Elm.withType
                        (Annotation.function
                            [ attributesType, instancesType, childrenType, propertiesType_ ]
                            (elementAnnotation (Annotation.var "msg"))
                        )
                    |> Elm.declaration declarationNameWithInstances
           , if List.isEmpty namesWithList then
                Elm.fn2 attributesParam
                    propertiesParam
                    (\attributes properties_ ->
                        componentFunction
                            attributes
                            (Elm.value
                                { importFrom = []
                                , name = declarationNameInstances
                                , annotation = Nothing
                                }
                            )
                            properties_
                            declarationNameWithInstances
                    )
                    |> Elm.withType
                        (Annotation.function
                            [ attributesType, propertiesType_ ]
                            (elementAnnotation (Annotation.var "msg"))
                        )
                    |> Elm.declaration declarationNameWithAttributes

             else
                Elm.fn3 attributesParam
                    childrenParam
                    propertiesParam
                    (\attributes children properties_ ->
                        componentFunctionWithChildren
                            attributes
                            (Elm.value
                                { importFrom = []
                                , name = declarationNameInstances
                                , annotation = Nothing
                                }
                            )
                            children
                            properties_
                            declarationNameWithInstances
                    )
                    |> Elm.withType
                        (Annotation.function
                            [ attributesType, childrenType, propertiesType_ ]
                            (elementAnnotation (Annotation.var "msg"))
                        )
                    |> Elm.declaration declarationNameWithAttributes
           , if List.isEmpty namesWithList then
                Elm.fn propertiesParam
                    (\properties_ ->
                        componentFunction
                            (Elm.value
                                { importFrom = []
                                , name = declarationNameAttributes
                                , annotation = Nothing
                                }
                            )
                            (Elm.value
                                { importFrom = []
                                , name = declarationNameInstances
                                , annotation = Nothing
                                }
                            )
                            properties_
                            declarationNameWithInstances
                    )
                    |> Elm.withType
                        (Annotation.function
                            [ propertiesType_ ]
                            (elementAnnotation (Annotation.var "msg"))
                        )
                    |> Elm.declaration declarationName

             else
                Elm.fn2 childrenParam
                    propertiesParam
                    (\children properties_ ->
                        componentFunctionWithChildren
                            (Elm.value
                                { importFrom = []
                                , name = declarationNameAttributes
                                , annotation = Nothing
                                }
                            )
                            (Elm.value
                                { importFrom = []
                                , name = declarationNameInstances
                                , annotation = Nothing
                                }
                            )
                            children
                            properties_
                            declarationNameWithInstances
                    )
                    |> Elm.withType
                        (Annotation.function
                            [ childrenType, propertiesType_ ]
                            (elementAnnotation (Annotation.var "msg"))
                        )
                    |> Elm.declaration declarationName
           ]
        ++ componentNodesForComponentSet formatSpecifics colorMap propertiesWithoutVariants sanitizedChildrenNode name


variantNameToComparisionExpression : Expression -> String -> String -> Expression
variantNameToComparisionExpression propertiesRecord parentName name =
    String.split ", " name
        |> List.filterMap
            (\part ->
                case String.split "=" part of
                    prop :: value :: [] ->
                        (parentName
                            ++ " "
                            ++ prop
                            ++ " "
                            ++ value
                            |> toCamelCaseUpper
                            |> Elm.val
                        )
                            |> Elm.Op.equal
                                (Elm.get rootName propertiesRecord |> Elm.get (sanitize prop))
                            |> Just

                    _ ->
                        Nothing
            )
        |> List.Extra.foldl1 Elm.Op.and
        |> Maybe.withDefault (Elm.bool False)


variantPropertyToType : String -> String -> List String -> Elm.Declaration
variantPropertyToType componentName name options =
    options
        |> List.map ((++) (componentName ++ " " ++ name ++ " ") >> toCamelCaseUpper >> Elm.variant)
        |> Elm.customType (componentName ++ " " ++ name |> toCamelCaseUpper)


componentNodeToDeclarations : FormatSpecifics -> ColorMap -> String -> Dict String (Dict String ComponentPropertyType) -> ComponentNodeOrSet -> List Elm.Declaration
componentNodeToDeclarations { toStyles, withFrameTraitsNodeToExpression, elementAnnotation, attributeAnnotation } colorMap parentName parentProperties node =
    let
        ( details, descendantsDetails ) =
            componentNodeToDetails node

        ( styles, descendantsStyles ) =
            toStyles colorMap node.frameTraits

        names =
            rootName
                :: List.map .name descendantsDetails

        properties =
            componentNodeToProperties details.name node
                |> Dict.union parentProperties

        propertiesType_ =
            properties
                |> propertiesType details.name elementAnnotation

        propertiesParamName =
            "properties"

        propertiesParam =
            ( propertiesParamName
            , Just propertiesType_
            )

        attributesType =
            names
                |> List.map (\n -> ( n, attributeAnnotation (Annotation.var "msg") |> Annotation.list ))
                |> Annotation.extensible "a"

        attributesParam =
            ( "attributes"
            , attributesType
                |> Just
            )

        instancesType =
            names
                |> List.map
                    (\n ->
                        ( n
                        , elementAnnotation (Annotation.var "msg")
                            |> Annotation.maybe
                        )
                    )
                |> Annotation.extensible "i"

        instancesParam =
            ( "instances"
            , instancesType
                |> Just
            )

        namesWithList =
            details.name
                :: List.map .name descendantsDetails
                |> List.filter FrameTraits.nameIsList

        childrenType =
            namesWithList
                |> List.map
                    (\n ->
                        ( n
                        , elementAnnotation (Annotation.var "msg")
                            |> Annotation.list
                        )
                    )
                |> Annotation.record

        childrenParam =
            ( "children"
            , childrenType
                |> Just
            )

        declarationName =
            parentName
                ++ " "
                ++ details.name

        declarationNameWithAttributes =
            declarationName ++ " with attributes" |> sanitize

        declarationNameWithInstances =
            declarationName
                ++ " with instances"
                |> sanitize

        declarationNameProperties =
            declarationName ++ " properties" |> sanitize

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
            [ propertiesType_
                |> Elm.alias declarationNameProperties
            , names
                |> defaultAttributeConfig attributeAnnotation
                |> Elm.declaration declarationNameAttributes
            , names
                |> defaultInstancesConfig elementAnnotation
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
                                    propertiesToPropertyExpressions properties_ properties
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
                        withFrameTraitsNodeToExpression config details.name rootName node.frameTraits
                    )
                    |> Elm.withType
                        (Annotation.function
                            [ attributesType, instancesType, propertiesType_ ]
                            (elementAnnotation (Annotation.var "msg"))
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
                                    propertiesToPropertyExpressions properties_ properties
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
                        withFrameTraitsNodeToExpression config details.name rootName node.frameTraits
                    )
                    |> Elm.withType
                        (Annotation.function
                            [ attributesType, instancesType, childrenType, propertiesType_ ]
                            (elementAnnotation (Annotation.var "msg"))
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
                            [ propertiesType_ ]
                            (elementAnnotation (Annotation.var "msg"))
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
                            [ childrenType, propertiesType_ ]
                            (elementAnnotation (Annotation.var "msg"))
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
                            [ attributesType, propertiesType_ ]
                            (elementAnnotation (Annotation.var "msg"))
                        )
                    |> Elm.declaration declarationNameWithAttributes

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
                            [ attributesType, childrenType, propertiesType_ ]
                            (elementAnnotation (Annotation.var "msg"))
                        )
                    |> Elm.declaration declarationNameWithAttributes
            ]


subcanvasNodeFilter : SubcanvasNode -> Maybe SubcanvasNode
subcanvasNodeFilter node =
    case node of
        SubcanvasNodeComponentNode n ->
            if FrameTraits.isHidden n then
                Nothing

            else
                filterUnneededParts n
                    |> SubcanvasNodeComponentNode
                    |> Just

        SubcanvasNodeComponentSetNode n ->
            if FrameTraits.isHidden n then
                Nothing

            else
                filterUnneededParts n
                    |> SubcanvasNodeComponentSetNode
                    |> Just

        SubcanvasNodeGroupNode n ->
            if FrameTraits.isHidden n then
                Nothing

            else
                filterUnneededParts n
                    |> SubcanvasNodeGroupNode
                    |> Just

        SubcanvasNodeFrameNode n ->
            if FrameTraits.isHidden n then
                Nothing

            else
                filterUnneededParts n
                    |> SubcanvasNodeFrameNode
                    |> Just

        SubcanvasNodeInstanceNode n ->
            if FrameTraits.isHidden n then
                Nothing

            else if hasVariantProperty n || hasMainComponentProperty n then
                s_children [] n.frameTraits
                    |> flip s_frameTraits n
                    |> SubcanvasNodeInstanceNode
                    |> Just

            else
                filterUnneededParts n
                    |> SubcanvasNodeInstanceNode
                    |> Just

        SubcanvasNodeTextNode n ->
            if DefaultShapeTraits.isHidden n then
                Nothing

            else
                Just node

        SubcanvasNodeRectangleNode n ->
            if DefaultShapeTraits.isHidden n.rectangularShapeTraits then
                Nothing

            else
                Just node

        SubcanvasNodeVectorNode n ->
            if DefaultShapeTraits.isHidden n.cornerRadiusShapeTraits then
                Nothing

            else
                Just node

        SubcanvasNodeLineNode n ->
            if DefaultShapeTraits.isHidden n then
                Nothing

            else
                Just node

        SubcanvasNodeEllipseNode n ->
            if DefaultShapeTraits.isHidden n then
                Nothing

            else
                Just node

        _ ->
            Nothing


filterUnneededParts : { a | frameTraits : FrameTraits } -> { a | frameTraits : FrameTraits }
filterUnneededParts a =
    a.frameTraits.children
        |> List.filterMap subcanvasNodeFilter
        |> flip s_children a.frameTraits
        |> flip s_frameTraits a


adjustBoundingBoxes : { a | frameTraits : FrameTraits } -> { a | frameTraits : FrameTraits }
adjustBoundingBoxes node =
    let
        originAdjust =
            getOriginAdjust node
    in
    withFrameTraitsAdjustBoundingBox originAdjust node


adjustNames : { a | frameTraits : FrameTraits } -> { a | frameTraits : FrameTraits }
adjustNames node =
    let
        names =
            collectNames [] node Dict.empty
                |> Dict.map (\_ -> List.map sanitize)
                |> disambiguateCollectedNames
                |> Dict.map (\_ -> sanitize)
    in
    withFrameTraitsAdjustNames names node


disambiguateCollectedNames : Dict String (List String) -> Dict String String
disambiguateCollectedNames dict =
    let
        list id prefix =
            Dict.toList dict
                |> List.filter (first >> (/=) id)
                |> List.filter (second >> List.Extra.isPrefixOf prefix)
                |> List.map second

        dis prefix index id names =
            let
                level =
                    List.length prefix
            in
            case names of
                fst :: rest ->
                    let
                        prefix2 =
                            prefix ++ [ fst ]
                    in
                    if List.any (List.Extra.getAt level >> Maybe.map sanitize >> (==) (Just (sanitize fst))) (list id prefix2) then
                        if level == 0 then
                            fst ++ dis prefix2 index id rest

                        else
                            dis prefix2 index id rest

                    else if level == 0 then
                        fst

                    else
                        " of " ++ fst

                [] ->
                    String.fromInt index
    in
    Dict.toList dict
        |> List.indexedMap (\i ( id, names ) -> ( id, dis [] i id names ))
        |> Dict.fromList


collectName : List String -> DefaultShapeTraits -> Dict String (List String) -> Dict String (List String)
collectName parentNames child =
    Dict.insert
        (DefaultShapeTraits.getId child)
        (DefaultShapeTraits.getName child :: parentNames)


collectNames : List String -> { a | frameTraits : FrameTraits } -> Dict String (List String) -> Dict String (List String)
collectNames parentNames node names =
    let
        name =
            FrameTraits.getName node.frameTraits

        newParentNames =
            name :: parentNames
    in
    node.frameTraits.children
        |> List.foldl
            (\child dict ->
                case child of
                    SubcanvasNodeTextNode n ->
                        collectName newParentNames n.defaultShapeTraits dict

                    SubcanvasNodeEllipseNode n ->
                        collectName newParentNames n.defaultShapeTraits dict

                    SubcanvasNodeComponentNode n ->
                        collectComponentNames newParentNames n dict

                    SubcanvasNodeComponentSetNode n ->
                        collectComponentSetNames newParentNames n dict

                    SubcanvasNodeFrameNode n ->
                        collectFrameNames newParentNames n dict

                    SubcanvasNodeGroupNode n ->
                        collectGroupNames newParentNames n dict

                    SubcanvasNodeInstanceNode n ->
                        collectInstanceNames newParentNames n dict

                    SubcanvasNodeVectorNode n ->
                        collectName newParentNames n.cornerRadiusShapeTraits.defaultShapeTraits dict

                    SubcanvasNodeLineNode n ->
                        collectName newParentNames n.defaultShapeTraits dict

                    SubcanvasNodeRectangleNode n ->
                        collectName newParentNames n.rectangularShapeTraits.defaultShapeTraits dict

                    _ ->
                        dict
            )
            (Dict.insert (FrameTraits.getId node) newParentNames names)


collectGroupNames : List String -> GroupNode -> Dict String (List String) -> Dict String (List String)
collectGroupNames =
    collectNames


collectFrameNames : List String -> FrameNode -> Dict String (List String) -> Dict String (List String)
collectFrameNames =
    collectNames


collectComponentNames : List String -> ComponentNode -> Dict String (List String) -> Dict String (List String)
collectComponentNames =
    collectNames


collectComponentSetNames : List String -> ComponentSetNode -> Dict String (List String) -> Dict String (List String)
collectComponentSetNames =
    collectNames


collectInstanceNames : List String -> InstanceNode -> Dict String (List String) -> Dict String (List String)
collectInstanceNames names node collected =
    if hasMainComponentProperty node || hasVariantProperty node then
        Dict.insert (FrameTraits.getId node) (FrameTraits.getName node.frameTraits :: names) collected

    else
        collectNames names node collected


subcanvasNodeAdjustBoundingBox : OriginAdjust -> SubcanvasNode -> SubcanvasNode
subcanvasNodeAdjustBoundingBox adjust node =
    case node of
        SubcanvasNodeTextNode n ->
            DefaultShapeTraits.adjustBoundingBox adjust n.defaultShapeTraits
                |> flip s_defaultShapeTraits n
                |> SubcanvasNodeTextNode

        SubcanvasNodeEllipseNode n ->
            DefaultShapeTraits.adjustBoundingBox adjust n.defaultShapeTraits
                |> flip s_defaultShapeTraits n
                |> SubcanvasNodeEllipseNode

        SubcanvasNodeComponentNode n ->
            -- applied when wrapped in ComponentSet
            let
                originAdjust =
                    getOriginAdjust n
            in
            withFrameTraitsAdjustBoundingBox originAdjust n
                |> SubcanvasNodeComponentNode

        SubcanvasNodeFrameNode n ->
            withFrameTraitsAdjustBoundingBox adjust n
                |> SubcanvasNodeFrameNode

        SubcanvasNodeGroupNode n ->
            withFrameTraitsAdjustBoundingBox adjust n
                |> SubcanvasNodeGroupNode

        SubcanvasNodeInstanceNode n ->
            withFrameTraitsAdjustBoundingBox adjust n
                |> SubcanvasNodeInstanceNode

        SubcanvasNodeVectorNode n ->
            VectorNode.adjustBoundingBox adjust n
                |> SubcanvasNodeVectorNode

        SubcanvasNodeLineNode n ->
            DefaultShapeTraits.adjustBoundingBox adjust n.defaultShapeTraits
                |> flip s_defaultShapeTraits n
                |> SubcanvasNodeLineNode

        SubcanvasNodeRectangleNode n ->
            RectangleNode.adjustBoundingBox adjust n
                |> SubcanvasNodeRectangleNode

        a ->
            a


subcanvasNodeAdjustNames : Dict String String -> SubcanvasNode -> SubcanvasNode
subcanvasNodeAdjustNames names node =
    case node of
        SubcanvasNodeTextNode n ->
            DefaultShapeTraits.adjustName names n.defaultShapeTraits
                |> flip s_defaultShapeTraits n
                |> SubcanvasNodeTextNode

        SubcanvasNodeEllipseNode n ->
            DefaultShapeTraits.adjustName names n.defaultShapeTraits
                |> flip s_defaultShapeTraits n
                |> SubcanvasNodeEllipseNode

        SubcanvasNodeComponentNode n ->
            withFrameTraitsAdjustNames names n
                |> SubcanvasNodeComponentNode

        SubcanvasNodeComponentSetNode n ->
            withFrameTraitsAdjustNames names n
                |> SubcanvasNodeComponentSetNode

        SubcanvasNodeFrameNode n ->
            withFrameTraitsAdjustNames names n
                |> SubcanvasNodeFrameNode

        SubcanvasNodeGroupNode n ->
            withFrameTraitsAdjustNames names n
                |> SubcanvasNodeGroupNode

        SubcanvasNodeInstanceNode n ->
            withFrameTraitsAdjustNames names n
                |> SubcanvasNodeInstanceNode

        SubcanvasNodeVectorNode n ->
            VectorNode.adjustName names n
                |> SubcanvasNodeVectorNode

        SubcanvasNodeLineNode n ->
            DefaultShapeTraits.adjustName names n.defaultShapeTraits
                |> flip s_defaultShapeTraits n
                |> SubcanvasNodeLineNode

        SubcanvasNodeRectangleNode n ->
            RectangleNode.adjustName names n
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


withFrameTraitsAdjustNames : Dict String String -> { a | frameTraits : FrameTraits } -> { a | frameTraits : FrameTraits }
withFrameTraitsAdjustNames names node =
    node.frameTraits.children
        |> List.map (subcanvasNodeAdjustNames names)
        |> flip s_children node.frameTraits
        |> flip s_frameTraits node
        |> FrameTraits.adjustName names


getOriginAdjust : { a | frameTraits : FrameTraits } -> OriginAdjust
getOriginAdjust node =
    node.frameTraits.absoluteBoundingBox
        |> (\r ->
                { x = r.x
                , y = r.y
                }
           )


formatComponentPropertyName : String -> String
formatComponentPropertyName str =
    if String.contains "#" str then
        String.Extra.leftOf "#" str

    else
        str


componentNodeToProperties : String -> ComponentNodeOrSet -> Dict String (Dict String ComponentPropertyType)
componentNodeToProperties name node =
    node.componentPropertiesTrait.componentPropertyDefinitions
        |> Maybe.map
            (Dict.toList
                >> List.map (mapSecond .type_)
                >> List.filter (instancePropertyIsNotPartOfAList node)
                >> Dict.fromList
            )
        |> Maybe.map (pair rootName >> List.singleton)
        |> Maybe.withDefault []
        |> (++)
            (withFrameTraitsToProperties node)
        |> Dict.fromList


instancePropertyIsNotPartOfAList : { a | frameTraits : FrameTraits } -> ( String, ComponentPropertyType ) -> Bool
instancePropertyIsNotPartOfAList node ( name, type_ ) =
    let
        withFrameTraits : Bool -> { b | frameTraits : FrameTraits } -> Bool
        withFrameTraits isList_ n =
            let
                newIsList =
                    isList_ || FrameTraits.isList n
            in
            List.all (walk newIsList) n.frameTraits.children

        walk isList_ nd =
            case nd of
                SubcanvasNodeFrameNode n ->
                    withFrameTraits isList_ n

                SubcanvasNodeGroupNode n ->
                    withFrameTraits isList_ n

                SubcanvasNodeInstanceNode n ->
                    isList_
                        && (getMainComponentProperty n.frameTraits.isLayerTrait.componentPropertyReferences
                                == Just name
                           )
                        |> not

                _ ->
                    True
    in
    if type_ /= ComponentPropertyTypeINSTANCESWAP then
        True

    else
        List.all (walk False) node.frameTraits.children


withFrameTraitsToProperties : { a | frameTraits : FrameTraits } -> List ( String, Dict String ComponentPropertyType )
withFrameTraitsToProperties node =
    if FrameTraits.isList node then
        []

    else
        node.frameTraits.children
            |> List.map subcanvasNodeToProperties
            |> List.concat


hasVariantProperty : InstanceNode -> Bool
hasVariantProperty n =
    n.componentProperties
        |> Maybe.withDefault Dict.empty
        |> Dict.values
        |> List.map .type_
        |> List.any ((==) ComponentPropertyTypeVARIANT)


hasMainComponentProperty : InstanceNode -> Bool
hasMainComponentProperty n =
    getMainComponentProperty n.frameTraits.isLayerTrait.componentPropertyReferences
        /= Nothing


subcanvasNodeToProperties : SubcanvasNode -> List ( String, Dict String ComponentPropertyType )
subcanvasNodeToProperties node =
    case node of
        SubcanvasNodeComponentNode n ->
            withFrameTraitsToProperties n

        SubcanvasNodeInstanceNode n ->
            if hasMainComponentProperty n then
                []

            else if hasVariantProperty n then
                [ ( FrameTraits.getName n.frameTraits
                  , Dict.singleton "variant" ComponentPropertyTypeINSTANCESWAP
                  )
                ]

            else
                n.componentProperties
                    |> Maybe.map
                        (Dict.toList
                            >> List.map (mapSecond .type_)
                            >> Dict.fromList
                            >> pair (FrameTraits.getName n.frameTraits)
                            >> List.singleton
                        )
                    |> Maybe.withDefault []
                    |> (++) (withFrameTraitsToProperties n)

        SubcanvasNodeFrameNode n ->
            withFrameTraitsToProperties n

        SubcanvasNodeGroupNode n ->
            withFrameTraitsToProperties n

        _ ->
            []


propertiesToPropertyExpressions : Expression -> Dict String (Dict String ComponentPropertyType) -> Dict String ComponentPropertyExpressions
propertiesToPropertyExpressions properties_ =
    Dict.map
        (\componentName ->
            Dict.map
                (\nam _ ->
                    properties_
                        |> Elm.get (sanitize componentName)
                        |> Elm.get (formatComponentPropertyName nam |> sanitize)
                )
        )


propertiesType : String -> (Annotation -> Annotation) -> Dict String (Dict String ComponentPropertyType) -> Annotation
propertiesType componentName elementType =
    Dict.map
        (\_ ->
            Dict.foldl
                (\k type_ ->
                    case type_ of
                        ComponentPropertyTypeBOOLEAN ->
                            Dict.insert k Annotation.bool

                        ComponentPropertyTypeINSTANCESWAP ->
                            Dict.insert k (elementType (Annotation.var "msg"))

                        ComponentPropertyTypeTEXT ->
                            Dict.insert k Annotation.string

                        ComponentPropertyTypeVARIANT ->
                            componentName
                                ++ " "
                                ++ k
                                |> toCamelCaseUpper
                                |> Annotation.named []
                                |> Dict.insert k
                )
                Dict.empty
                >> Dict.toList
                >> List.map (mapFirst (formatComponentPropertyName >> sanitize))
                >> Annotation.record
        )
        >> Dict.toList
        >> List.map (mapFirst sanitize)
        >> Annotation.record


disambiguateNames : Dict ( String, String ) v -> Dict ( String, String ) v
disambiguateNames properties =
    let
        counts =
            properties
                |> Dict.keys
                -- the name/id tuple
                |> List.map first
                -- the name
                |> List.foldl
                    (\i -> Dict.update i (Maybe.map ((+) 1) >> Maybe.withDefault 1 >> Just))
                    Dict.empty
    in
    properties
        |> Dict.Extra.mapKeys
            (\( name, id ) ->
                Dict.get name counts
                    |> Maybe.map
                        (\c ->
                            if c > 1 then
                                ( name, id )

                            else
                                ( name, "" )
                        )
                    |> Maybe.withDefault ( name, id )
            )


defaultAttributeConfig : (Annotation -> Annotation) -> List String -> Elm.Expression
defaultAttributeConfig attributeType =
    Set.fromList
        >> Set.toList
        >> List.map
            (\n ->
                ( sanitize n
                , Elm.list []
                    |> Elm.withType
                        (attributeType
                            (Annotation.var "msg")
                            |> Annotation.list
                        )
                )
            )
        >> Elm.record


defaultInstancesConfig : (Annotation -> Annotation) -> List String -> Elm.Expression
defaultInstancesConfig elementType =
    List.map
        (\n ->
            ( sanitize n
            , Elm.nothing
                |> Elm.withType
                    (elementType
                        (Annotation.var "msg")
                        |> Annotation.maybe
                    )
            )
        )
        >> Elm.record


wrapInSvg :
    Config
    -> String
    ->
        { a
            | absoluteRenderBounds : Maybe Rectangle
            , absoluteBoundingBox : Rectangle
            , layoutPositioning : Maybe LayoutPositioning
        }
    -> List Elm.Expression
    -> Elm.Expression
wrapInSvg config name { absoluteRenderBounds, absoluteBoundingBox, layoutPositioning } =
    let
        bbox =
            absoluteBoundingBox

        rbox =
            absoluteRenderBounds
                |> Maybe.withDefault bbox

        width =
            max 3 rbox.width
                |> String.fromFloat

        height =
            max 3 rbox.height
                |> String.fromFloat

        positionRelatively =
            DefaultShapeTraits.positionRelatively config
                { absoluteBoundingBox = absoluteBoundingBox }
    in
    Elm.list
        >> Gen.Svg.Styled.call_.svg
            ([ width
                |> Gen.Svg.Styled.Attributes.width
             , height
                |> Gen.Svg.Styled.Attributes.height
             , [ bbox.x
               , bbox.y
               , max 3 rbox.width
               , max 3 rbox.height
               ]
                |> List.map String.fromFloat
                |> String.join " "
                |> Gen.Svg.Styled.Attributes.viewBox
             ]
                |> Elm.list
            )
        >> List.singleton
        >> Elm.list
        >> Gen.Html.Styled.call_.div
            (getElementAttributes config name
                |> Elm.Op.append
                    (mm2 HasLayoutTrait.layoutPositioning layoutPositioning (Just absoluteBoundingBox) []
                        ++ positionRelatively
                        ++ [ width ++ "px" |> Css.property "width"
                           , height ++ "px" |> Css.property "height"
                           ]
                        |> Gen.Svg.Styled.Attributes.css
                        |> List.singleton
                        |> Elm.list
                    )
            )


componentNodeToDetails : { a | frameTraits : FrameTraits } -> ( Details, List Details )
componentNodeToDetails node =
    ( FrameTraits.toDetails node
    , node.frameTraits.children
        |> List.map subcanvasNodeToDetails
        |> List.concat
    )


withFrameTraitsNodeToDetails : { a | frameTraits : FrameTraits } -> ( Details, List Details )
withFrameTraitsNodeToDetails node =
    ( FrameTraits.toDetails node
    , if FrameTraits.isList node then
        []

      else
        node.frameTraits.children
            |> List.map subcanvasNodeToDetails
            |> List.concat
    )


subcanvasNodeToDetails : SubcanvasNode -> List Details
subcanvasNodeToDetails node =
    case node of
        SubcanvasNodeTextNode n ->
            if DefaultShapeTraits.isHidden n then
                []

            else
                DefaultShapeTraits.toDetails n.defaultShapeTraits
                    |> List.singleton

        SubcanvasNodeComponentNode n ->
            if FrameTraits.isHidden n then
                []

            else
                withFrameTraitsNodeToDetails n
                    |> mapFirst (s_name rootName)
                    |> uncurry (::)

        SubcanvasNodeGroupNode n ->
            if FrameTraits.isHidden n then
                []

            else
                withFrameTraitsNodeToDetails n
                    |> uncurry (::)

        SubcanvasNodeFrameNode n ->
            if FrameTraits.isHidden n then
                []

            else
                withFrameTraitsNodeToDetails n
                    |> uncurry (::)

        SubcanvasNodeInstanceNode n ->
            if FrameTraits.isHidden n then
                []

            else if hasVariantProperty n || hasMainComponentProperty n then
                []

            else
                withFrameTraitsNodeToDetails n
                    |> uncurry (::)
                    |> List.map (s_instanceName (FrameTraits.getName n.frameTraits))

        SubcanvasNodeRectangleNode n ->
            if DefaultShapeTraits.isHidden n.rectangularShapeTraits then
                []

            else
                DefaultShapeTraits.toDetails n.rectangularShapeTraits.defaultShapeTraits
                    |> List.singleton

        SubcanvasNodeVectorNode n ->
            if DefaultShapeTraits.isHidden n.cornerRadiusShapeTraits then
                []

            else
                DefaultShapeTraits.toDetails n.cornerRadiusShapeTraits.defaultShapeTraits
                    |> List.singleton

        SubcanvasNodeLineNode n ->
            if DefaultShapeTraits.isHidden n then
                []

            else
                DefaultShapeTraits.toDetails n.defaultShapeTraits
                    |> List.singleton

        SubcanvasNodeEllipseNode n ->
            if DefaultShapeTraits.isHidden n then
                []

            else
                DefaultShapeTraits.toDetails n.defaultShapeTraits
                    |> List.singleton

        _ ->
            []

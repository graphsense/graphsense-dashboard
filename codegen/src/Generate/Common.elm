module Generate.Common exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (flip, uncurry)
import Dict exposing (Dict)
import Dict.Extra
import Elm exposing (Expression)
import Elm.Annotation as Annotation exposing (Annotation)
import Elm.Case
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
import Generate.Util exposing (getElementAttributes, getMainComponentProperty, mm2, rootName, sanitize)
import List.Extra
import RecordSetter exposing (s_children, s_defaultShapeTraits, s_frameTraits, s_instanceName, s_name)
import Set
import String.Case exposing (toCamelCaseUpper)
import String.Extra
import Tuple exposing (first, mapFirst, mapSecond, pair, second)
import Types exposing (ComponentPropertyExpressions, Config, Details, OriginAdjust)


subcanvasNodeComponentsToDeclarations : (String -> Dict String (Dict String ComponentPropertyType) -> ComponentNodeOrSet -> List Elm.Declaration) -> SubcanvasNode -> List Elm.Declaration
subcanvasNodeComponentsToDeclarations componentNodeToDeclarations node =
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
                    |> componentNodeToDeclarations ""
                        Dict.empty

        SubcanvasNodeComponentSetNode n ->
            if FrameTraits.isHidden n then
                []

            else
                let
                    filtered =
                        filter n

                    cns =
                        filtered
                            |> toComponentNodeOrSet
                in
                componentSetNodeToDeclarations componentNodeToDeclarations cns

        _ ->
            []


toComponentNodeOrSet : { a | componentPropertiesTrait : ComponentPropertiesTrait, frameTraits : FrameTraits } -> ComponentNodeOrSet
toComponentNodeOrSet n =
    { componentPropertiesTrait = n.componentPropertiesTrait
    , frameTraits = n.frameTraits
    }


componentNodesForComponentSet : (String -> Dict String (Dict String ComponentPropertyType) -> ComponentNodeOrSet -> List Elm.Declaration) -> Dict String (Dict String ComponentPropertyType) -> ComponentNodeOrSet -> String -> List Elm.Declaration
componentNodesForComponentSet toDeclarations parentProperties n parentName =
    n.frameTraits.children
        |> List.map
            (\child ->
                case child of
                    SubcanvasNodeComponentNode nn ->
                        adjustNames nn
                            |> toComponentNodeOrSet
                            |> toDeclarations
                                parentName
                                parentProperties

                    _ ->
                        []
            )
        |> List.concat


type alias ComponentNodeOrSet =
    { componentPropertiesTrait : ComponentPropertiesTrait, frameTraits : FrameTraits }


componentSetNodeToDeclarations : (String -> Dict String (Dict String ComponentPropertyType) -> ComponentNodeOrSet -> List Elm.Declaration) -> ComponentNodeOrSet -> List Elm.Declaration
componentSetNodeToDeclarations toDeclarations node =
    let
        ( details, descendantsDetails ) =
            componentNodeToDetails node

        names =
            rootName
                :: List.map .name descendantsDetails
                |> Set.fromList
                |> Set.toList

        attributesType =
            names
                |> List.map (\n -> ( n, Gen.Html.Styled.annotation_.attribute (Annotation.var "msg") |> Annotation.list ))
                |> Annotation.record

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

        name =
            FrameTraits.getName node
                |> sanitize

        properties =
            componentNodeToProperties name node

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
                                |> Maybe.map (pair k)

                        else
                            Just ( k, types )
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

        propertiesType_old =
            variantProperties
                |> List.map (mapSecond (\_ -> ComponentPropertyTypeVARIANT))
                |> Dict.fromList
                |> Dict.singleton name
                |> propertiesType name Gen.Html.Styled.annotation_.html

        propertiesType_ =
            properties
                |> propertiesType name Gen.Html.Styled.annotation_.html

        propertiesParam =
            ( "properties"
            , Just propertiesType_
            )

        switches : Expression -> List ( String, Expression )
        switches propertiesRecord =
            variantProperties
                |> List.map
                    (\( prop, values ) ->
                        List.map
                            (\value ->
                                ( prop ++ "=" ++ value
                                , (name
                                    ++ " "
                                    ++ prop
                                    ++ " "
                                    ++ value
                                    |> toCamelCaseUpper
                                    |> Elm.val
                                  )
                                    |> Elm.Op.equal
                                        (Elm.get prop propertiesRecord)
                                )
                            )
                            values
                    )
                |> List.Extra.cartesianProduct
                -- combine the comparisons
                |> List.filterMap
                    (List.Extra.foldl1
                        (\( na, a ) ( nb, b ) ->
                            ( na ++ " " ++ nb
                            , Elm.Op.and a b
                            )
                        )
                    )

        declarationNameAttributes =
            name ++ " attributes" |> sanitize

        declarationNameInstances =
            name ++ " instances" |> sanitize

        componentFunction propertiesRecord nam =
            Elm.apply
                (Elm.value
                    { importFrom = []
                    , name = name ++ " " ++ nam ++ " with instances" |> sanitize
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
                , propertiesRecord
                ]
    in
    (propertiesType_
        |> Elm.alias (name ++ " properties" |> sanitize)
    )
        :: (variantProperties |> List.map (uncurry (variantPropertyToType name)))
        ++ [ names
                |> defaultAttributeConfig Gen.Html.Styled.annotation_.attribute
                |> Elm.declaration declarationNameAttributes
           , names
                |> defaultInstancesConfig Gen.Html.Styled.annotation_.html
                |> Elm.declaration declarationNameInstances
           ]
        ++ ((Elm.fn propertiesParam
                (\properties_ ->
                    Elm.Let.letIn
                        (\propsWithoutVariants ->
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
                                        List.foldl
                                            (\comp acc ->
                                                let
                                                    nam =
                                                        FrameTraits.getName comp
                                                in
                                                Elm.ifThen
                                                    (variantNameToComparisionExpression properties_ name nam)
                                                    (componentFunction propsWithoutVariants nam)
                                                    acc
                                            )
                                            (FrameTraits.getName fst |> componentFunction propsWithoutVariants)
                                            rest
                                    )
                                |> Maybe.withDefault (Html.text "")
                        )
                        |> Elm.Let.value "propsWithoutVariants"
                            (propertiesWithoutVariants
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
                            )
                        |> Elm.Let.toExpression
                )
                |> Elm.withType
                    (Annotation.function
                        [ propertiesType_ ]
                        (Gen.Html.Styled.annotation_.html (Annotation.var "msg"))
                    )
                |> Elm.declaration name
            )
                :: componentNodesForComponentSet toDeclarations propertiesWithoutVariants node name
           )


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
                                (Elm.get rootName propertiesRecord |> Elm.get prop)
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
            FrameTraits.getName node

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
        Dict.insert (FrameTraits.getId node) (FrameTraits.getName node :: names) collected

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
        SubcanvasNodeInstanceNode n ->
            if hasMainComponentProperty n then
                []

            else if hasVariantProperty n then
                [ ( FrameTraits.getName n
                  , Dict.singleton "variant" ComponentPropertyTypeINSTANCESWAP
                  )
                ]

            else
                n.componentProperties
                    |> Maybe.map
                        (Dict.toList
                            >> List.map (mapSecond .type_)
                            >> Dict.fromList
                            >> pair (FrameTraits.getName n)
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
                    |> List.map (s_instanceName (FrameTraits.getName n))

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

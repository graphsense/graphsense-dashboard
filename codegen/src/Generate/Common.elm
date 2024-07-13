module Generate.Common exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Dict.Extra
import Elm exposing (Expression)
import Elm.Annotation as Annotation exposing (Annotation)
import Generate.Common.DefaultShapeTraits as DefaultShapeTraits
import Generate.Common.FrameTraits as FrameTraits
import Generate.Common.RectangleNode as RectangleNode
import Generate.Common.VectorNode as VectorNode
import Generate.Util exposing (sanitize)
import List.Extra
import RecordSetter exposing (s_children, s_frameTraits)
import Set
import String.Extra
import Tuple exposing (first, mapFirst, mapSecond, pair, second)
import Types exposing (ComponentPropertyExpressions, OriginAdjust)


adjustBoundingBoxes : ComponentNode -> ComponentNode
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
                |> Debug.log "asdf collectNames"
                |> disambiguateCollectedNames
                |> Dict.map (\_ -> sanitize)
                |> Debug.log "asdf disambiguateCollectNames"
    in
    withFrameTraitsAdjustNames names node


disambiguateCollectedNames : Dict String (List String) -> Dict String String
disambiguateCollectedNames dict =
    let
        list id =
            Dict.toList dict
                |> List.filter (first >> (/=) id)
                |> List.map second

        dis level index id names =
            case names of
                fst :: rest ->
                    if List.any (List.Extra.getAt level >> (==) (Just fst)) (list id) then
                        if level == 0 then
                            fst ++ " of " ++ dis (level + 1) index id rest

                        else
                            dis (level + 1) index id rest

                    else
                        fst

                [] ->
                    String.fromInt index
    in
    Dict.toList dict
        |> List.indexedMap (\i ( id, names ) -> ( id, dis 0 i id names ))
        |> Dict.fromList


collectName : List String -> { a | defaultShapeTraits : DefaultShapeTraits } -> Dict String (List String) -> Dict String (List String)
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
                        collectName newParentNames n dict

                    SubcanvasNodeEllipseNode n ->
                        collectName newParentNames n dict

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
                        collectName newParentNames n.cornerRadiusShapeTraits dict

                    SubcanvasNodeLineNode n ->
                        collectName newParentNames n dict

                    SubcanvasNodeRectangleNode n ->
                        collectName newParentNames n.rectangularShapeTraits dict

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
collectInstanceNames =
    collectNames


subcanvasNodeAdjustBoundingBox : OriginAdjust -> SubcanvasNode -> SubcanvasNode
subcanvasNodeAdjustBoundingBox adjust node =
    case node of
        SubcanvasNodeTextNode n ->
            DefaultShapeTraits.adjustBoundingBox adjust n
                |> SubcanvasNodeTextNode

        SubcanvasNodeEllipseNode n ->
            DefaultShapeTraits.adjustBoundingBox adjust n
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
            DefaultShapeTraits.adjustBoundingBox adjust n
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
            DefaultShapeTraits.adjustName names n
                |> SubcanvasNodeTextNode

        SubcanvasNodeEllipseNode n ->
            DefaultShapeTraits.adjustName names n
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
            DefaultShapeTraits.adjustName names n
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


getOriginAdjust : ComponentNode -> OriginAdjust
getOriginAdjust node =
    node.frameTraits.absoluteBoundingBox
        |> (\r ->
                { x = r.x
                , y = r.y
                }
           )


formatComponentPropertyName : String -> String
formatComponentPropertyName =
    String.Extra.leftOf "#"


componentNodeToProperties : String -> ComponentNode -> Dict String (Dict String ComponentPropertyType)
componentNodeToProperties name node =
    node.componentPropertiesTrait.componentPropertyDefinitions
        |> Maybe.map
            (Dict.toList
                >> List.map
                    (mapSecond .type_)
                >> Dict.fromList
            )
        |> Maybe.map (pair name >> List.singleton)
        |> Maybe.withDefault []
        |> (++)
            (withFrameTraitsToProperties node)
        |> Dict.fromList


withFrameTraitsToProperties : { a | frameTraits : FrameTraits } -> List ( String, Dict String ComponentPropertyType )
withFrameTraitsToProperties node =
    node.frameTraits.children
        |> List.map subcanvasNodeToProperties
        |> List.concat


subcanvasNodeToProperties : SubcanvasNode -> List ( String, Dict String ComponentPropertyType )
subcanvasNodeToProperties node =
    case node of
        SubcanvasNodeInstanceNode n ->
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


propertiesType : (Annotation -> Annotation) -> Dict String (Dict String ComponentPropertyType) -> Annotation
propertiesType elementType =
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
                            identity
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

module Generate.Common exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (flip, uncurry)
import Dict exposing (Dict)
import Dict.Extra
import Elm exposing (Expression)
import Elm.Annotation as Annotation exposing (Annotation)
import Generate.Common.DefaultShapeTraits as DefaultShapeTraits
import Generate.Common.FrameTraits as FrameTraits
import Generate.Common.RectangleNode as RectangleNode
import Generate.Common.VectorNode as VectorNode
import Generate.Util exposing (sanitize)
import RecordSetter exposing (s_children, s_frameTraits)
import String.Extra
import Tuple exposing (first, mapBoth, mapFirst, mapSecond, pair)
import Types exposing (ComponentPropertyExpressions, OriginAdjust)


adjustBoundingBoxes : ComponentNode -> ComponentNode
adjustBoundingBoxes node =
    let
        originAdjust =
            getOriginAdjust node
    in
    withFrameTraitsAdjustBoundingBox originAdjust node


subcanvasNodeAdjustBoundingBox : OriginAdjust -> SubcanvasNode -> SubcanvasNode
subcanvasNodeAdjustBoundingBox adjust node =
    case node of
        SubcanvasNodeTextNode n ->
            DefaultShapeTraits.adjustBoundingBox adjust n
                |> SubcanvasNodeTextNode

        SubcanvasNodeEllipseNode n ->
            DefaultShapeTraits.adjustBoundingBox adjust n
                |> SubcanvasNodeEllipseNode

        SubcanvasNodeGroupNode n ->
            withFrameTraitsAdjustBoundingBox adjust n
                |> SubcanvasNodeGroupNode

        SubcanvasNodeInstanceNode n ->
            withFrameTraitsAdjustBoundingBox adjust n
                |> SubcanvasNodeInstanceNode

        SubcanvasNodeVectorNode n ->
            VectorNode.adjustBoundingBox adjust n
                |> SubcanvasNodeVectorNode

        SubcanvasNodeRectangleNode n ->
            RectangleNode.adjustBoundingBox adjust n
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


componentNodeToProperties : String -> ComponentNode -> Dict ( String, String ) (Dict String ComponentPropertyType)
componentNodeToProperties name node =
    ( ( name, "" )
    , node.componentPropertiesTrait.componentPropertyDefinitions
        |> Maybe.map
            (Dict.toList
                >> List.map
                    (mapSecond .type_)
                >> Dict.fromList
            )
        |> Maybe.withDefault Dict.empty
    )
        :: withFrameTraitsToProperties node
        |> Dict.fromList


withFrameTraitsToProperties : { a | frameTraits : FrameTraits } -> List ( ( String, String ), Dict String ComponentPropertyType )
withFrameTraitsToProperties node =
    node.frameTraits.children
        |> List.map subcanvasNodeToProperties
        |> List.concat


subcanvasNodeToProperties : SubcanvasNode -> List ( ( String, String ), Dict String ComponentPropertyType )
subcanvasNodeToProperties node =
    case node of
        SubcanvasNodeInstanceNode n ->
            n.componentProperties
                |> Maybe.map
                    (Dict.toList
                        >> List.map (mapSecond .type_)
                        >> Dict.fromList
                        >> pair ( FrameTraits.getName n, FrameTraits.getId n )
                        >> List.singleton
                    )
                |> Maybe.withDefault []
                |> (++) (withFrameTraitsToProperties n)
        SubcanvasNodeFrameNode n ->
            withFrameTraitsToProperties n

        _ ->
            []


propertiesToPropertyExpressions : Expression -> Dict ( String, String ) (Dict String ComponentPropertyType) -> Dict ( String, String ) ComponentPropertyExpressions
propertiesToPropertyExpressions properties_ =
    disambiguateNames
        >> Dict.map
            (\( componentName, id ) ->
                Dict.map
                    (\nam _ ->
                        properties_
                            |> Elm.get (sanitizeTuple componentName id)
                            |> Elm.get (formatComponentPropertyName nam |> sanitize)
                    )
            )


sanitizeTuple : String -> String -> String
sanitizeTuple componentName id =
    sanitize <| componentName ++ " " ++ id


propertiesType : (Annotation -> Annotation) -> Dict ( String, String ) (Dict String ComponentPropertyType) -> Annotation
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
        >> disambiguateNames
        >> Dict.toList
        >> List.map (mapFirst (uncurry sanitizeTuple))
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

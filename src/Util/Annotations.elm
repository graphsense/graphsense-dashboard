module Util.Annotations exposing (AnnotationItem, AnnotationModel, Group, annotationToAttrAndLabel, empty, expandToGroups, getAnnotation, getVisibleAnnotation, groups, harmonizeGroups, newGroupId, set, setColor, setGroup, setLabel, toList)

import Animation as A
import Basics.Extra exposing (flip)
import Color exposing (Color)
import Config.View as View
import Css
import Dict exposing (Dict)
import List.Extra
import Model.Pathfinder.Id exposing (Id)
import RecordSetter as Rs
import Set
import String.Format
import Svg.Styled as Svg exposing (Svg, path, text)
import Svg.Styled.Attributes as Svg exposing (css, opacity, transform)
import Theme.Colors as Colors
import Theme.Svg.GraphComponents as GraphComponents
import Tuple exposing (pair, second)
import Update.Pathfinder.Node exposing (Node)
import Util.Graph exposing (translate)
import Util.TextDimensions exposing (estimateTextWidth)
import Util.View exposing (onClickWithStop)


type AnnotationModel
    = Annotation ModelInternal


type alias ModelInternal =
    { annotations : Dict Id AnnotationItem
    }


type alias AnnotationItem =
    { label : String
    , color : Maybe Color
    , groupId : Maybe Int
    }


{-| A group of nodes sharing the same groupId. Rendered as a single colored
box behind its members instead of per-node annotations.
-}
type alias Group =
    { id : Int
    , members : List Id
    , label : String
    , color : Maybe Color
    }


empty : AnnotationModel
empty =
    { annotations = Dict.empty } |> Annotation


toList : AnnotationModel -> List ( Id, AnnotationItem )
toList (Annotation m) =
    Dict.toList m.annotations


defaultAnnotation : AnnotationItem
defaultAnnotation =
    { label = "", color = Nothing, groupId = Nothing }


set : Id -> String -> Maybe Color -> AnnotationModel -> AnnotationModel
set item lbl clr (Annotation m) =
    { m
        | annotations =
            Dict.insert item
                ((Dict.get item m.annotations
                    |> Maybe.withDefault defaultAnnotation
                 )
                    |> Rs.s_label lbl
                    |> Rs.s_color clr
                )
                m.annotations
    }
        |> Annotation


setLabel : Id -> String -> AnnotationModel -> AnnotationModel
setLabel item lbl (Annotation m) =
    { m | annotations = Dict.insert item ((Dict.get item m.annotations |> Maybe.withDefault defaultAnnotation) |> Rs.s_label lbl) m.annotations } |> Annotation


setColor : Id -> Maybe Color -> AnnotationModel -> AnnotationModel
setColor item clr (Annotation m) =
    { m | annotations = Dict.insert item ((Dict.get item m.annotations |> Maybe.withDefault defaultAnnotation) |> Rs.s_color clr) m.annotations } |> Annotation


setGroup : Id -> Maybe Int -> AnnotationModel -> AnnotationModel
setGroup item gid (Annotation m) =
    { m | annotations = Dict.insert item ((Dict.get item m.annotations |> Maybe.withDefault defaultAnnotation) |> Rs.s_groupId gid) m.annotations } |> Annotation


getAnnotation : Id -> AnnotationModel -> Maybe AnnotationItem
getAnnotation item (Annotation m) =
    Dict.get item m.annotations


{-| The annotation as it should be rendered on the node. Grouped nodes carry
a lightened version of the group color (to show membership) and no per-node
label — the label belongs on the group box.
-}
getVisibleAnnotation : Id -> AnnotationModel -> Maybe AnnotationItem
getVisibleAnnotation item annotations =
    getAnnotation item annotations
        |> Maybe.map
            (\ann ->
                if ann.groupId == Nothing then
                    ann

                else
                    { ann
                        | label = ""
                        , color = Maybe.map lighten ann.color
                    }
            )


{-| Mix a color toward white — used to tint grouped nodes lighter than a
regular annotation.
-}
lighten : Color -> Color
lighten color =
    let
        c =
            Color.toHsla color
    in
    Color.fromHsla { c | lightness = c.lightness + (1 - c.lightness) * 0.6 }


{-| Expand a list of ids with all their group-mates, so an edit made on one
grouped node propagates to the whole group.
-}
expandToGroups : List Id -> AnnotationModel -> List Id
expandToGroups ids (Annotation m) =
    let
        targetGroups =
            ids
                |> List.filterMap (\id -> Dict.get id m.annotations |> Maybe.andThen .groupId)
                |> Set.fromList
    in
    if Set.isEmpty targetGroups then
        ids

    else
        m.annotations
            |> Dict.toList
            |> List.filterMap
                (\( id, ann ) ->
                    ann.groupId
                        |> Maybe.andThen
                            (\g ->
                                if Set.member g targetGroups then
                                    Just id

                                else
                                    Nothing
                            )
                )
            |> (++) ids
            |> List.Extra.unique


{-| Force every group's members to share one label and color (the group's
derived ones), so grouped annotations stay consistent.
-}
harmonizeGroups : AnnotationModel -> AnnotationModel
harmonizeGroups annotations =
    groups annotations
        |> List.concatMap (\g -> List.map (\id -> ( id, g.label, g.color )) g.members)
        |> List.foldl (\( id, lbl, clr ) -> set id lbl clr) annotations


{-| The next free group id: one above the highest existing one. Computed from
the current annotations so it survives save/load and never collides.
-}
newGroupId : AnnotationModel -> Int
newGroupId (Annotation m) =
    m.annotations
        |> Dict.values
        |> List.filterMap .groupId
        |> List.maximum
        |> Maybe.withDefault 0
        |> (+) 1


{-| All groups with their member node ids. Label and color are taken from the
members (the dialog sets them for all selected nodes together).
-}
groups : AnnotationModel -> List Group
groups (Annotation m) =
    Dict.toList m.annotations
        |> List.filterMap (\( id, ann ) -> ann.groupId |> Maybe.map (\gid -> ( gid, ( id, ann ) )))
        |> List.Extra.gatherEqualsBy Tuple.first
        |> List.map
            (\( first, rest ) ->
                let
                    members =
                        first :: rest

                    items =
                        List.map (Tuple.second >> Tuple.second) members
                in
                { id = Tuple.first first
                , members = List.map (Tuple.second >> Tuple.first) members
                , label =
                    items
                        |> List.map .label
                        |> List.Extra.find (String.isEmpty >> not)
                        |> Maybe.withDefault ""
                , color =
                    items
                        |> List.filterMap .color
                        |> List.head
                }
            )


annotationToAttrAndLabel : View.Config -> Node a -> { b | height : Float, width : Float } -> Float -> (Id -> msg) -> AnnotationItem -> ( List (Svg.Attribute msg), List (Svg msg) )
annotationToAttrAndLabel vc node details offset msg ann =
    let
        colorAttributes prop =
            case ann.color of
                Just c ->
                    Color.toCssString c
                        |> Css.property prop
                        |> Css.important
                        |> List.singleton
                        |> css
                        |> List.singleton

                _ ->
                    []

        aw =
            GraphComponents.addressNode_details.width

        words =
            ann.label
                |> String.split " "
                |> List.filter (String.isEmpty >> not)

        charDimHere =
            -- to account for font-weight 600 of annotation text
            Dict.map
                (\_ d -> { d | width = d.width * 1.1 })
                vc.characterDimensions

        dimensions =
            words
                |> List.map (\word -> ( word, estimateTextWidth charDimHere word ))

        maxHeight =
            vc.characterDimensions
                |> Dict.values
                |> List.map .height
                |> List.maximum
                -- should never happen ...
                |> Maybe.withDefault 10

        maxTextWidth =
            dimensions
                |> List.map second
                |> List.maximum
                |> Maybe.withDefault 0

        maxWidth =
            maxTextWidth
                |> max aw

        lines =
            -- calculate line breaks
            dimensions
                |> List.foldl
                    (\( word, wordWidth ) ( currentWidth, lines_ ) ->
                        if currentWidth + wordWidth < maxWidth then
                            lines_
                                |> List.Extra.last
                                |> Maybe.withDefault ""
                                |> flip (++) (" " ++ word)
                                |> List.singleton
                                |> (++) (List.take (List.length lines_ - 1) lines_)
                                |> pair (currentWidth + wordWidth)

                        else
                            ( wordWidth, lines_ ++ [ word ] )
                    )
                    ( 0, [] )
                |> second

        annotationWidth =
            lines
                |> List.map (estimateTextWidth charDimHere)
                |> List.maximum
                |> Maybe.withDefault 0

        textLines =
            lines
                |> List.indexedMap
                    (\i line ->
                        Svg.text_
                            [ css GraphComponents.annotationLabel2Label_details.styles
                            , Svg.textAnchor "middle"
                            , Svg.dominantBaseline "hanging"
                            , annotationWidth / 2 |> String.fromFloat |> Svg.x
                            , toFloat i * maxHeight |> String.fromFloat |> Svg.y
                            ]
                            [ text line ]
                    )

        paddingX =
            4

        paddingY =
            3
    in
    ( colorAttributes "fill"
    , (if String.length ann.label > 0 then
        [ roundedRectangle
            { width =
                annotationWidth
                    + paddingX
                    * 2
            , height =
                maxHeight
                    * toFloat (List.length lines)
                    + paddingY
                    * 2
            , radius = 1
            }
            [ GraphComponents.annotationLabel_details.strokeWidth
                |> String.fromFloat
                |> Svg.strokeWidth
            , ann.color
                |> Maybe.map Color.toCssString
                |> Maybe.withDefault Colors.brandBlack
                |> Svg.stroke
            , Svg.fill "transparent"
            ]
        , textLines
            |> Svg.g
                [ translate paddingX (paddingY + GraphComponents.annotationLabel_details.strokeWidth) |> transform
                ]
        ]
            |> Svg.g
                [ translate
                    ((annotationWidth - details.width) / -2 - paddingX)
                    (details.height
                        + offset
                    )
                    |> transform
                , A.animate node.clock node.opacity
                    |> String.fromFloat
                    |> opacity
                , msg node.id |> onClickWithStop
                , css
                    [ Css.cursor Css.pointer
                    , Css.pointerEvents Css.none
                    ]
                ]

       else
        text ""
      )
        |> List.singleton
    )


roundedRectangle : { width : Float, height : Float, radius : Float } -> List (Svg.Attribute msg) -> Svg msg
roundedRectangle { width, height, radius } attrs =
    let
        -- Calculate the path data for the rounded rectangle using String.format
        pathData =
            "M {{ radius }} ,0 "
                ++ "L {{ wmr }} ,0 "
                ++ "Q {{ width }} ,0 {{ width }} ,{{ radius }}  "
                ++ "L {{ width }} ,{{ hmr }}  "
                ++ "Q {{ width }} ,{{ height }}  {{ wmr }} ,{{ height }}  "
                ++ "L {{ radius }} ,{{ height }}  "
                ++ "Q 0,{{ height }}  0,{{ hmr }}  "
                ++ "L 0,{{ radius }}  "
                ++ "Q 0,0 {{ radius }} ,0 "
                ++ "Z"
                |> String.Format.namedValue "radius" (String.fromFloat radius)
                |> String.Format.namedValue "width" (String.fromFloat width)
                |> String.Format.namedValue "height" (String.fromFloat height)
                |> String.Format.namedValue "wmr" (String.fromFloat (width - radius))
                |> String.Format.namedValue "hmr" (String.fromFloat (height - radius))
    in
    path
        (Svg.d pathData :: attrs)
        []

module Util.Annotations exposing (AnnotationItem, AnnotationModel, annotationToAttrAndLabel, empty, getAnnotation, set, setColor, setLabel, toList)

import Animation as A
import Color exposing (Color)
import Css
import Dict exposing (Dict)
import Html.Styled
import Model.Pathfinder.Id exposing (Id)
import RecordSetter as Rs
import Svg.Styled as Svg exposing (Svg, text)
import Svg.Styled.Attributes as Svg exposing (css, opacity, transform)
import Theme.Html.GraphComponents as HtmlGraphComponents
import Theme.Svg.GraphComponents as GraphComponents
import Update.Pathfinder.Node exposing (Node)
import Util.Graph exposing (translate)
import Util.View exposing (onClickWithStop)


type AnnotationModel
    = Annotation ModelInternal


type alias ModelInternal =
    { annotations : Dict Id AnnotationItem
    }


type alias AnnotationItem =
    { label : String
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
    { label = "", color = Nothing }


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


getAnnotation : Id -> AnnotationModel -> Maybe AnnotationItem
getAnnotation item (Annotation m) =
    Dict.get item m.annotations


annotationToAttrAndLabel : Node a -> { b | height : Float, width : Float } -> Float -> (Id -> msg) -> AnnotationItem -> ( List (Svg.Attribute msg), List (Svg msg) )
annotationToAttrAndLabel node details offset msg ann =
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
    in
    ( colorAttributes "fill"
    , (if String.length ann.label > 0 then
        HtmlGraphComponents.annotationLabelWithAttributes
            (HtmlGraphComponents.annotationLabelAttributes
                |> Rs.s_root
                    (css
                        [ Css.display Css.inlineBlock
                        ]
                        :: colorAttributes "border-color"
                    )
            )
            { root = { labelText = ann.label } }
            |> List.singleton
            |> Html.Styled.div
                [ css
                    [ Css.pct 100 |> Css.width
                    , Css.textAlign Css.center
                    , Css.position Css.fixed
                    ]
                ]
            |> List.singleton
            |> Svg.foreignObject
                [ translate
                    ((aw - details.width) / -2)
                    (details.height
                        + offset
                    )
                    |> transform
                , aw
                    |> String.fromFloat
                    |> Svg.width
                , (GraphComponents.annotationLabel_details.height
                    + GraphComponents.annotationLabel_details.strokeWidth
                    * 2
                    + 2
                  )
                    * (1 + (toFloat <| String.length ann.label // 12))
                    |> String.fromFloat
                    |> Svg.height
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

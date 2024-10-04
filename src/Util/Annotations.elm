module Util.Annotations exposing (AnnotationItem, AnnotationModel, empty, getAnnotation, set, setColor, setLabel, toList)

import Color exposing (Color)
import Dict exposing (Dict)
import Model.Pathfinder.Id exposing (Id)
import RecordSetter as Rs


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
    { m | annotations = Dict.insert item ((Dict.get item m.annotations |> Maybe.withDefault defaultAnnotation) |> Rs.s_label lbl |> Rs.s_color clr) m.annotations } |> Annotation


setLabel : Id -> String -> AnnotationModel -> AnnotationModel
setLabel item lbl (Annotation m) =
    { m | annotations = Dict.insert item ((Dict.get item m.annotations |> Maybe.withDefault defaultAnnotation) |> Rs.s_label lbl) m.annotations } |> Annotation


setColor : Id -> Maybe Color -> AnnotationModel -> AnnotationModel
setColor item clr (Annotation m) =
    { m | annotations = Dict.insert item ((Dict.get item m.annotations |> Maybe.withDefault defaultAnnotation) |> Rs.s_color clr) m.annotations } |> Annotation


getAnnotation : Id -> AnnotationModel -> Maybe AnnotationItem
getAnnotation item (Annotation m) =
    Dict.get item m.annotations

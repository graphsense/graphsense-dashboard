module Util.Annotations exposing (AnnotationItem, AnnotationModel, empty, getAnnotation, setColor, setLabel)

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


defaultAnnotation : AnnotationItem
defaultAnnotation =
    { label = "", color = Nothing }


setLabel : Id -> String -> AnnotationModel -> AnnotationModel
setLabel item lbl (Annotation m) =
    { m | annotations = Dict.insert item ((Dict.get item m.annotations |> Maybe.withDefault defaultAnnotation) |> Rs.s_label lbl) m.annotations } |> Annotation


setColor : Id -> Color -> AnnotationModel -> AnnotationModel
setColor item clr (Annotation m) =
    { m | annotations = Dict.insert item ((Dict.get item m.annotations |> Maybe.withDefault defaultAnnotation) |> Rs.s_color (Just clr)) m.annotations } |> Annotation


getAnnotation : Id -> AnnotationModel -> Maybe AnnotationItem
getAnnotation item (Annotation m) =
    Dict.get item m.annotations

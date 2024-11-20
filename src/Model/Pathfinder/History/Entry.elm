module Model.Pathfinder.History.Entry exposing (Model)

import Model.Pathfinder.Network exposing (Network)
import Util.Annotations exposing (AnnotationModel)


type alias Model =
    { network : Network
    , annotations : AnnotationModel
    }

module Model.Graph.Highlighter exposing (..)

import Color exposing (Color)
import List.Extra
import Tuple exposing (..)


type alias Model =
    { highlights : List ( String, Color )
    , selected : Maybe Int
    }


getSelectedColor : Model -> Maybe Color
getSelectedColor model =
    model.selected
        |> Maybe.andThen (\s -> List.Extra.getAt s model.highlights)
        |> Maybe.map second


getColor : Int -> Model -> Maybe Color
getColor i model =
    List.Extra.getAt i model.highlights
        |> Maybe.map second

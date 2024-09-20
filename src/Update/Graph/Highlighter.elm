module Update.Graph.Highlighter exposing (deselect, removeHighlight, selectColor, selectHighlight, setHighlightTitle)

import Color exposing (Color)
import List.Extra
import Model.Graph.Highlighter exposing (..)
import Tuple exposing (..)


selectColor : Color -> Model -> Model
selectColor color model =
    { model
        | highlights =
            ( "", color ) :: model.highlights
        , selected = Just 0
    }


removeHighlight : Int -> Model -> Model
removeHighlight i model =
    { model
        | highlights =
            List.Extra.removeAt i model.highlights
        , selected =
            if Just i == model.selected then
                Nothing

            else
                model.selected
    }


setHighlightTitle : Int -> String -> Model -> Model
setHighlightTitle i title model =
    { model
        | highlights =
            List.Extra.updateAt i (mapFirst (always title)) model.highlights
    }


selectHighlight : Int -> Model -> Model
selectHighlight i model =
    { model
        | selected =
            if model.selected == Just i then
                Nothing

            else
                Just i
    }


deselect : Model -> Model
deselect model =
    { model | selected = Nothing }

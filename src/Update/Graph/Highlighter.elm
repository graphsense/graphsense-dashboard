module Update.Graph.Highlighter exposing (..)

import Color exposing (Color)
import List.Extra
import Model.Graph.Highlighter exposing (..)
import Tuple exposing (..)


selectColor : Color -> Model -> Model
selectColor color model =
    case model.selected of
        Nothing ->
            { model
                | highlights =
                    ( "", color ) :: model.highlights
                , selected = Just 0
            }

        Just i ->
            { model
                | highlights =
                    List.Extra.updateAt i (mapSecond (always color)) model.highlights
            }


removeHighlight : Int -> Model -> Model
removeHighlight i model =
    { model
        | highlights =
            List.Extra.removeAt i model.highlights
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

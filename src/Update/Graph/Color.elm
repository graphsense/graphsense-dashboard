module Update.Graph.Color exposing (..)

import Color exposing (Color)
import Config.Update as Update
import Dict exposing (Dict)
import List.Extra


update : Update.Config -> Dict String Color -> Maybe String -> Dict String Color
update { defaultColor, colorScheme } colors category =
    category
        |> Maybe.map
            (\cat ->
                Dict.update cat
                    (Maybe.withDefault
                        (List.Extra.getAt
                            (Dict.size colors |> modBy (List.length colorScheme))
                            colorScheme
                            |> Maybe.withDefault defaultColor
                        )
                        >> Just
                    )
                    colors
            )
        |> Maybe.withDefault colors



-- Additionally, this one receives a categoryToColorIndex dictionary
-- this way, we can update the colors of the graph based on the category
-- instead of iterating over all the colors, also it doesnt need the colors dict


updateWithIndex : Update.Config -> Dict String Color -> Maybe String -> Dict String Color
updateWithIndex { defaultColor, colorScheme, categoryToColorIndex } colors category =
    case category of
        Just cat ->
            let
                color =
                    Dict.get cat categoryToColorIndex
                        |> Maybe.andThen (\index -> List.Extra.getAt index colorScheme)
                        |> Maybe.withDefault defaultColor
            in
            Dict.insert cat color colors

        Nothing ->
            colors

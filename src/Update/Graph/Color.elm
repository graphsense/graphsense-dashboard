module Update.Graph.Color exposing (..)

import Color exposing (Color)
import Config.Update as Update
import Config.View as View
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

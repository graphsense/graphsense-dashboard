module Update.Graph.History exposing (..)

import Config.Graph.History as Config
import List.Extra
import Model.Graph.History exposing (Entry, Model)


undo : Model -> Entry -> Maybe ( Model, Entry )
undo model current =
    model.past
        |> List.Extra.uncons
        |> Maybe.map
            (\( recent, past ) ->
                ( { model
                    | past = past
                    , future = current :: model.future
                  }
                , recent
                )
            )


redo : Model -> Entry -> Maybe ( Model, Entry )
redo model current =
    model.future
        |> List.Extra.uncons
        |> Maybe.map
            (\( recent, future ) ->
                ( { model
                    | past = current :: model.past
                    , future = future
                  }
                , recent
                )
            )


prune : Model -> Model
prune model =
    { model
        | past = List.take Config.maxLength model.past
    }


push : Model -> Entry -> Model
push model entry =
    { past = entry :: model.past
    , future = []
    }

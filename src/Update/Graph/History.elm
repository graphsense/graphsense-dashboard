module Update.Graph.History exposing (prune, push, redo, undo)

import Config.Graph.History as Config
import List.Extra
import Model.Graph.History exposing (Model)
import Model.Graph.History.Entry as Entry


undo : Model entry -> entry -> Maybe ( Model entry, entry )
undo model current =
    prune model current
        |> .past
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


redo : Model entry -> entry -> Maybe ( Model entry, entry )
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


prune : Model entry -> entry -> Model entry
prune model current =
    let
        filter old =
            case old of
                fst :: rest ->
                    if current == fst then
                        filter rest

                    else
                        old

                [] ->
                    []
    in
    { model
        | past = List.take Config.maxLength <| filter model.past
    }


push : Model entry -> entry -> Model entry
push model entry =
    { past =
        if List.head model.past == Just entry then
            model.past

        else
            entry :: model.past
    , future = []
    }

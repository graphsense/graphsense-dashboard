module Update.Pathfinder.AddTagDialog exposing (update)

import Config.Update as Update
import Maybe.Extra
import Model exposing (AddTagDialogMsgs(..), Effect(..), Msg(..))
import Model.Dialog exposing (AddTagConfig)
import Model.Search as Search
import Msg.Search as Search
import RecordSetter as Rs
import Update.Search as Search


update : Update.Config -> AddTagDialogMsgs -> AddTagConfig msg -> ( AddTagConfig msg, List Effect )
update _ msg model =
    case msg of
        SearchMsgAddTagDialog m ->
            let
                searchm =
                    model.search

                ( searchNew, searchEffects ) =
                    Search.update m searchm

                selected =
                    case m of
                        Search.UserClicksResultLine ->
                            let
                                -- query =
                                --     Search.query model.search
                                selectedValue =
                                    Search.selectedValue model.search
                                        |> Maybe.Extra.orElse (Search.firstResult model.search)
                            in
                            case selectedValue of
                                Just (Search.Actor ref) ->
                                    Just ref

                                _ ->
                                    model.selectedActor

                        _ ->
                            model.selectedActor
            in
            ( model |> Rs.s_search searchNew |> Rs.s_selectedActor selected
            , List.map (SearchEffect (SearchMsgAddTagDialog >> AddTagDialog)) searchEffects
            )

        UserInputsDescription ns ->
            ( model |> Rs.s_description ns
            , []
            )

        RemoveActorTag ->
            ( model |> Rs.s_selectedActor Nothing, [] )

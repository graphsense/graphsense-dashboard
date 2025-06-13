module Update.Pathfinder.AddTagDialog exposing (update)

import Config.Update as Update
import Maybe.Extra
import Model exposing (AddTagDialogMsgs(..), Effect(..), Msg(..))
import Model.Dialog exposing (AddTagConfig)
import Model.Search as Search
import Msg.Search as Search
import RecordSetter as Rs
import Update.Search as Search
import View.Locale as Locale


update : Update.Config -> AddTagDialogMsgs -> AddTagConfig msg -> ( AddTagConfig msg, List Effect )
update uc msg model =
    case msg of
        SearchMsgAddTagDialog m ->
            let
                addNewId =
                    "addNewTag"

                ( searchNews, searchEffects ) =
                    Search.update m model.search

                last =
                    Search.Custom { id = addNewId, label = Locale.string uc.locale "Create new actor" }

                addConst =
                    searchNews |> Search.lastResult |> Maybe.map ((/=) last) |> Maybe.withDefault True

                searchNew =
                    if addConst then
                        searchNews |> Search.addToAutoComplete last

                    else
                        searchNews

                selected =
                    case m of
                        Search.UserClicksResultLine ->
                            let
                                query =
                                    Search.query model.search

                                selectedValue =
                                    Search.selectedValue model.search
                                        |> Maybe.Extra.orElse (Search.firstResult model.search)
                            in
                            case selectedValue of
                                Just (Search.Actor ref) ->
                                    Just ref

                                Just (Search.Custom data) ->
                                    if data.id == addNewId then
                                        Just ( query, query )

                                    else
                                        model.selectedActor

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

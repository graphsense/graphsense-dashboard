module Update.Pathfinder.AddTagDialog exposing (update)

import Config.Update as Update
import Dict
import Effect.Api as Api
import Effect.Pathfinder as PFEffect
import Maybe.Extra
import Model exposing (AddTagDialogMsgs(..), Effect(..), Msg(..))
import Model.Dialog exposing (AddTagConfig)
import Model.Notification as Notification
import Model.Pathfinder.Id as Id
import Model.Search as Search
import Msg.Search as Search
import RecordSetter as Rs
import Update.Pathfinder as Pathfinder
import Update.Search as Search
import View.Locale as Locale


update : Update.Config -> AddTagDialogMsgs -> AddTagConfig Msg -> ( AddTagConfig Msg, List Effect )
update uc msg model =
    case msg of
        BrowserAddedTag id ->
            ( model
            , [ Notification.successDefault (Locale.string uc.locale "Added Tag")
                    |> Notification.map (Rs.s_isEphemeral True)
                    |> Notification.map (Rs.s_showClose False)
                    |> PFEffect.ShowNotificationEffect
                    |> PathfinderEffect
              , Pathfinder.fetchTagSummaryForId True Dict.empty id |> PathfinderEffect -- force tag refresh
              ]
            )

        UserClickedAddTag id ->
            case model.selectedActor of
                Just sa ->
                    let
                        tag =
                            { actor = sa |> Tuple.first
                            , address = Id.id id
                            , description = model.description
                            , label = sa |> Tuple.second
                            , network = Id.network id
                            }
                    in
                    ( model, ((BrowserAddedTag id |> AddTagDialog |> always) |> Api.AddUserReportedTag tag) |> ApiEffect |> List.singleton )

                Nothing ->
                    ( model
                    , Notification.errorDefault (Locale.string uc.locale "Tag is invalid")
                        |> Notification.map (Rs.s_isEphemeral True)
                        |> Notification.map (Rs.s_showClose False)
                        |> PFEffect.ShowNotificationEffect
                        |> PathfinderEffect
                        |> List.singleton
                    )

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

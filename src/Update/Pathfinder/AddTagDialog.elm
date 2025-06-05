module Update.Pathfinder.AddTagDialog exposing (update)

-- import Maybe.Extra
-- import Model.Search as Search
-- import Msg.Search as Search

import Config.Update as Update
import Model exposing (AddTagDialogMsgs(..), Effect(..), Msg(..))
import Model.Dialog exposing (AddTagConfig)
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

                -- a =
                --     case m of
                --         Search.UserClicksResultLine ->
                --             let
                --                 query =
                --                     Search.query model.search
                --                 selectedValue =
                --                     Search.selectedValue model.search
                --                         |> Maybe.Extra.orElse (Search.firstResult model.search)
                --             in
                --             True
                --         _ ->
                --             False
            in
            ( model |> Rs.s_search searchNew
            , List.map (SearchEffect (SearchMsgAddTagDialog >> AddTagDialog)) searchEffects
            )

        UserInputsDescription ns ->
            ( model |> Rs.s_description ns
            , []
            )

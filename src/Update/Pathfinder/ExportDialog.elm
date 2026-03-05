module Update.Pathfinder.ExportDialog exposing (update)

import Config.Update as Update
import Dict
import Effect.Api as Api
import Effect.Pathfinder as PFEffect
import Maybe.Extra
import Model exposing (Effect(..))
import Model.Dialog exposing (ExportConfig)
import Model.Notification as Notification
import Model.Pathfinder.Id as Id
import Model.Search as Search
import Msg.ExportDialog as ExportDialog exposing (..)
import Msg.Search as Search
import RecordSetter as Rs
import Update.Pathfinder as Pathfinder
import Update.Search as Search
import Util exposing (n)
import View.Locale as Locale


update : Update.Config -> ExportDialog.Msg -> ExportConfig Model.Msg -> ( ExportConfig Model.Msg, List Effect )
update uc msg model =
    case msg of
        UserClickedExport ->
            n model

module Update.Pathfinder.TxDetails exposing (update)

import Effect exposing (n)
import Effect.Pathfinder exposing (Effect)
import Model.Pathfinder.TxDetails exposing (..)
import Msg.Pathfinder exposing (TxDetailsMsg(..))
import RecordSetter exposing (s_state)


update : TxDetailsMsg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        UserClickedToggleIOTable ->
            n { model | ioTableOpen = not model.ioTableOpen }

        TableMsg state ->
            n { model | table = model.table |> s_state state }

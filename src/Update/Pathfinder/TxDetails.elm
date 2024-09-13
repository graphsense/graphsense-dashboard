module Update.Pathfinder.TxDetails exposing (update)

import Effect exposing (n)
import Effect.Pathfinder exposing (Effect)
import Model.Pathfinder.TxDetails exposing (..)
import Msg.Pathfinder exposing (IoDirection(..), TxDetailsMsg(..))
import RecordSetter exposing (s_state)


update : TxDetailsMsg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        UserClickedToggleIoTable Inputs ->
            n { model | inputsTableOpen = not model.inputsTableOpen }

        UserClickedToggleIoTable Outputs ->
            n { model | outputsTableOpen = not model.outputsTableOpen }

        TableMsg Inputs state ->
            n { model | inputsTable = model.inputsTable |> s_state state }

        TableMsg Outputs state ->
            n { model | outputsTable = model.outputsTable |> s_state state }

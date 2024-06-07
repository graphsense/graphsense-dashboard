module Update.Pathfinder.TxDetails exposing (update)

import Effect exposing (n)
import Effect.Pathfinder exposing (Effect)
import Model.Pathfinder exposing (TxDetailsViewState)
import Msg.Pathfinder exposing (TxDetailsMsg(..))


update : TxDetailsMsg -> TxDetailsViewState -> ( TxDetailsViewState, List Effect )
update msg model =
    case msg of
        UserClickedToggleIOTable ->
            n { model | ioTableOpen = not model.ioTableOpen }

module Update.Pathfinder.TxDetails exposing (update)

import Effect exposing (n)
import Effect.Pathfinder exposing (Effect)
import Model.Pathfinder.Details.TxDetails as TxDetails
import Msg.Pathfinder exposing (TxDetailsMsg(..))


update : TxDetailsMsg -> TxDetails.Model -> ( TxDetails.Model, List Effect )
update msg model =
    case msg of
        UserClickedToggleIOTable ->
            n { model | ioTableOpen = not model.ioTableOpen }

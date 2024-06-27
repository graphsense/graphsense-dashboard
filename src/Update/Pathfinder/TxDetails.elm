module Update.Pathfinder.TxDetails exposing (update)

import Effect exposing (n)
import Effect.Pathfinder exposing (Effect)
import Model.Pathfinder.TxDetails exposing (..)
import Msg.Pathfinder exposing (TxDetailsMsg(..))


update : TxDetailsMsg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        UserClickedToggleIOTable ->
            n { model | ioTableOpen = not model.ioTableOpen }

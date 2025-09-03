module Update.Pathfinder.ConversionDetails exposing (update)

import Effect.Pathfinder exposing (Effect)
import Model.Pathfinder.ConversionDetails exposing (ConversionDetailsModel)
import Msg.Pathfinder.ConversionDetails exposing (ConversionDetailsMsgs(..))


update : ConversionDetailsMsgs -> ConversionDetailsModel -> ( ConversionDetailsModel, List Effect )
update msg model =
    case msg of
        UserTogglesConversionLegTable ->
            ( { model | isConversionLegTableOpen = not model.isConversionLegTableOpen }, [] )

        UserClickedTxCheckboxInTable _ ->
            -- handled upstream
            ( model, [] )

module Msg.Pathfinder.ConversionDetails exposing (ConversionDetailsMsgs(..))

import Model.Pathfinder.Id exposing (Id)


type ConversionDetailsMsgs
    = UserTogglesConversionLegTable
    | UserClickedTxCheckboxInTable Id

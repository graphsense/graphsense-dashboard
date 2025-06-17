module Msg.Pathfinder.RelationDetails exposing (Msg(..))

import Api.Data
import Model.Pathfinder.Id exposing (Id)
import PagedTable


type Msg
    = UserClickedToggleTable Bool
    | TableMsg Bool PagedTable.Msg
    | BrowserGotLinks Bool Api.Data.Links
    | UserClickedAllTxCheckboxInTable Bool
    | UserClickedTxCheckboxInTable Api.Data.Link
    | UserClickedTx Id
    | NoOp

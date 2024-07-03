module View.Pathfinder.Tooltip exposing (view)

import Api.Data
import Config.View as View
import Html.Styled as Html exposing (..)
import Model.Pathfinder.Tooltip exposing (Tooltip(..))
import Model.Pathfinder.Tx as Tx
import Theme.Html.GraphComponents as GraphComponents exposing (defaultProperty1DownAttributes)
import Util.View exposing (none)


view : View.Config -> Tooltip -> Html msg
view vc tt =
    case tt of
        UtxoTx t ->
            utxoTx vc t


utxoTx : View.Config -> Tx.UtxoTx -> Html msg
utxoTx vc tx =
    none

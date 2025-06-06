module View.Pathfinder.AggEdge exposing (edge, view)

import Config.Pathfinder as Pathfinder
import Config.View as View
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.Tx exposing (Tx, TxType(..))
import Msg.Pathfinder exposing (Msg)
import Plugin.View exposing (Plugins)
import Svg.Styled exposing (Svg)
import Theme.Svg.GraphComponentsAggregatedTracing as Theme
import View.Locale as Locale
import Model.Pathfinder.Address exposing (Address)
import View.Pathfinder.Tx.Utils exposing (toPosition)
import RecordSetter exposing (s_root)
import Util.Graph exposing (translate)
import Model.Pathfinder exposing (unit)
import Svg.Styled.Attributes exposing (transform)


view : Plugins -> View.Config -> Pathfinder.Config -> AggEdge -> Address -> Address -> Svg Msg
view plugins vc gc ed fromAddress toAddress =
    let
        asset =
            { network = ed.data.address.currency, asset = ed.data.address.currency }

        fromPos = 
            fromAddress |> toPosition
        toPos = 
            toAddress |> toPosition

        x = (fromPos.x + toPos.x) / 2 
        y = (fromPos.y + toPos.y) / 2 
    in
    Theme.aggregatedValuesWithAttributes
        (Theme.aggregatedValuesAttributes
            |> s_root
                [ translate
                    (x * unit)
                    (y * unit)
                    |> transform
                ]
        )
        { root =
            { divider = "|"
            , firstValue =
                Locale.currencyWithoutCode vc.locale [ ( asset, ed.data.value ) ]
            , secondValue = ""
            , highlightVisible = False
            , leftArrowVisible = True
            , rightArrowVisible = True
            , secondValueVisible = False
            }
        }


edge : Plugins -> View.Config -> Pathfinder.Config -> AggEdge -> Svg Msg
edge plugins vc gc ed =
    Debug.todo ""

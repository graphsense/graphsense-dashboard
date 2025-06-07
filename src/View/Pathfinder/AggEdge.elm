module View.Pathfinder.AggEdge exposing (edge, view)

import Config.Pathfinder as Pathfinder
import Config.View as View
import Model.Pathfinder exposing (unit)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.Tx exposing (TxType(..))
import Msg.Pathfinder exposing (Msg)
import Plugin.View exposing (Plugins)
import RecordSetter exposing (s_root)
import Svg.Styled exposing (Svg)
import Svg.Styled.Attributes exposing (transform)
import Theme.Svg.GraphComponentsAggregatedTracing as Theme
import Util.Graph exposing (translate)
import View.Locale as Locale
import View.Pathfinder.Tx.Utils exposing (toPosition)


view : View.Config -> AggEdge -> Address -> Address -> Svg Msg
view vc ed fromAddress toAddress =
    let
        asset data =
            { network = data.address.currency, asset = data.address.currency }

        fromPos =
            fromAddress |> toPosition

        toPos =
            toAddress |> toPosition

        x =
            (fromPos.x + toPos.x) / 2

        y =
            (fromPos.y + toPos.y) / 2
    in
    Theme.aggregatedLabelWithAttributes
        (Theme.aggregatedLabelAttributes
            |> s_root
                [ translate
                    (x * unit)
                    (y * unit)
                    |> transform
                ]
        )
        { root =
            { leftValue =
                ed.toNeighborData
                    |> Maybe.map
                        (\data ->
                            Locale.currencyWithoutCode vc.locale [ ( asset data, data.value ) ]
                        )
                    |> Maybe.withDefault ""
            , rightValue =
                ed.fromNeighborData
                    |> Maybe.map
                        (\data ->
                            Locale.currencyWithoutCode vc.locale [ ( asset data, data.value ) ]
                        )
                    |> Maybe.withDefault ""
            , showHighlight = False
            }
        }


edge : Plugins -> View.Config -> Pathfinder.Config -> AggEdge -> Svg Msg
edge plugins vc gc ed =
    Debug.todo ""

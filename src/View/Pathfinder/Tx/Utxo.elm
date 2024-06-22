module View.Pathfinder.Tx.Utxo exposing (edge, view)

import Animation as A
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Css.Pathfinder as Css
import Dict exposing (Dict)
import Dict.Nonempty as NDict
import Model.Direction exposing (Direction(..))
import Model.Pathfinder exposing (unit)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Tx exposing (..)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.View as Plugin exposing (Plugins)
import Svg.PathD exposing (..)
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import Theme.PathfinderComponents as PathfinderComponents exposing (defaultTxNodeAttributes)
import Tuple exposing (pair, second)
import Util.Graph exposing (translate)
import Util.Pathfinder exposing (getAddress)
import View.Locale as Locale
import View.Pathfinder.Tx.Path exposing (inPath, outPath)


view : Plugins -> View.Config -> Pathfinder.Config -> Id -> UtxoTx -> Svg Msg
view _ vc _ id tx =
    let
        anyIsNotVisible =
            NDict.toList
                >> List.any (second >> .visible >> not)

        fd =
            PathfinderComponents.txNodeBodyEllipseDimensions

        adjX =
            fd.x + fd.width / 2

        adjY =
            fd.y + fd.height / 2
    in
    PathfinderComponents.txNode
        { defaultTxNodeAttributes
            | txNode =
                [ translate
                    ((tx.x + tx.dx) * unit - adjX)
                    ((A.animate tx.clock tx.y + tx.dy) * unit - adjY)
                    |> transform
                , A.animate tx.clock tx.opacity
                    |> String.fromFloat
                    |> opacity
                , UserClickedTx id |> onClick
                , UserPushesLeftMouseButtonOnUtxoTx id
                    |> Util.Graph.mousedown
                , css [ Css.cursor Css.pointer ]
                ]
        }
        { moreVisible = anyIsNotVisible tx.inputs || anyIsNotVisible tx.outputs
        , highlightVisible = tx.selected
        }


edge : Plugins -> View.Config -> Pathfinder.Config -> Dict Id Address -> UtxoTx -> Svg Msg
edge _ vc _ addresses tx =
    let
        toValues =
            NDict.toList
                >> List.filterMap
                    (\( id, { values } ) ->
                        getAddress addresses id
                            |> Result.toMaybe
                            |> Maybe.map
                                (values
                                    |> pair { network = Id.network id, asset = Id.network id }
                                    |> List.singleton
                                    |> Locale.currency vc.locale
                                    >> pair
                                )
                    )

        outputValues =
            tx.outputs
                |> toValues

        inputValues =
            tx.inputs
                |> toValues

        fd =
            PathfinderComponents.addressNodeFrameDimensions

        rad =
            fd.width / 2

        txRad =
            vc.theme.pathfinder.txRadius

        toCoords address =
            { tx = tx.x + tx.dx
            , ty = A.animate tx.clock tx.y + tx.dy
            , ax = address.x + address.dx
            , ay = A.animate address.clock address.y + address.dy
            }
    in
    (inputValues
        |> List.map
            (\( values, address ) ->
                let
                    c =
                        toCoords address

                    sign =
                        if c.ax > c.tx then
                            -1

                        else
                            1
                in
                ( Id.toString address.id
                , Svg.lazy7 inPath
                    vc
                    values
                    (c.ax * unit + (rad * sign))
                    (c.ay * unit)
                    (c.tx * unit - (txRad * sign))
                    (c.ty * unit)
                    (A.animate tx.clock tx.opacity)
                )
            )
    )
        ++ (outputValues
                |> List.map
                    (\( values, address ) ->
                        let
                            c =
                                toCoords address

                            sign =
                                if c.ax < c.tx then
                                    -1

                                else
                                    1
                        in
                        ( Id.toString address.id
                        , Svg.lazy7 outPath
                            vc
                            values
                            (c.tx * unit + (txRad * sign))
                            (c.ty * unit)
                            (c.ax * unit - (rad * sign))
                            (c.ay * unit)
                            (A.animate address.clock address.opacity)
                        )
                    )
           )
        |> Keyed.node "g"
            []

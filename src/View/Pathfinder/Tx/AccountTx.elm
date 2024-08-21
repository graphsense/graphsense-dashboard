module View.Pathfinder.Tx.AccountTx exposing (edge)

import Animation as A
import Config.Pathfinder as Pathfinder
import Config.View as View
import Dict exposing (Dict)
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
import Theme.Svg.GraphComponents as GraphComponents
import Tuple exposing (pair)
import Util.Pathfinder exposing (getAddress)
import View.Locale as Locale
import View.Pathfinder.Tx.Path exposing (accountPath)


edge : Plugins -> View.Config -> Pathfinder.Config -> Dict Id Address -> AccountTx -> Svg Msg
edge _ vc _ addresses tx =
    let
        rad =
            GraphComponents.addressNodeNodeFrameDetails.width / 2
    in
    tx.to
        |> getAddress addresses
        |> Result.toMaybe
        |> Maybe.map2
            (\fro too ->
                let
                    label =
                        tx.value
                            |> pair { network = Id.network fro.id, asset = tx.raw.currency }
                            |> List.singleton
                            |> Locale.currency vc.locale
                in
                accountPath
                    vc
                    label
                    ((fro.x + fro.dx) * unit + rad)
                    ((A.animate fro.clock fro.y + fro.dy) * unit)
                    ((too.x + too.dx) * unit - rad)
                    ((A.animate too.clock too.y + too.dy) * unit)
                    (A.animate fro.clock fro.opacity
                        |> Basics.min (A.animate too.clock too.opacity)
                    )
            )
            (tx.from
                |> getAddress addresses
                |> Result.toMaybe
            )
        |> Maybe.withDefault (text "")

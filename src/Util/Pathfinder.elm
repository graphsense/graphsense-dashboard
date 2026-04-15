module Util.Pathfinder exposing (getAddress, tooltipConfig)

import Components.Tooltip as Tooltip
import Config.View as View
import Dict exposing (Dict)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Error as PfError
import Model.Pathfinder.Id exposing (Id)
import Util.Css as Css


getAddress : Dict Id Address -> Id -> Result PfError.Error Address
getAddress addresses id =
    Dict.get id addresses
        |> Maybe.map Ok
        |> Maybe.withDefault (PfError.AddressNotFoundInDict id |> PfError.InternalError |> Err)


tooltipConfig : View.Config -> (Tooltip.Msg -> msg) -> Tooltip.Config msg
tooltipConfig vc tag =
    Tooltip.defaultConfig tag
        |> Tooltip.withZIndex (Css.zIndexMainValue + 10000)
        |> Tooltip.withBorderColor (vc.theme.hovercard vc.lightmode).borderColor
        |> Tooltip.withBackgroundColor (vc.theme.hovercard vc.lightmode).backgroundColor
        |> Tooltip.withBorderWidth (vc.theme.hovercard vc.lightmode).borderWidth

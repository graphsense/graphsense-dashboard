module View.Pathfinder.Address exposing (view)

import Config.Pathfinder as Pathfinder
import Config.View as View
import Model.Pathfinder.Address as Address exposing (..)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.View as Plugin exposing (Plugins)
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg


view : Plugins -> View.Config -> Pathfinder.Config -> Address -> Svg Msg
view plugins vc gc address =
    circle
        [ x <| String.fromFloat address.x
        , y <| String.fromFloat address.y
        , r <| String.fromFloat Pathfinder.addressRadius
        ]
        []

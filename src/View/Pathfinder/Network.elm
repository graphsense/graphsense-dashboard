module View.Pathfinder.Network exposing (addresses)

import Config.Pathfinder as Pathfinder
import Config.View as View
import Dict exposing (Dict)
import Model.Pathfinder exposing (..)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.Model exposing (ModelState)
import Plugin.View as Plugin exposing (Plugins)
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import View.Pathfinder.Address as Address


addresses : Plugins -> View.Config -> Pathfinder.Config -> Dict Id Address -> Svg Msg
addresses plugins vc gc =
    Dict.foldl
        (\id address svg ->
            ( Id.toString id
            , Svg.lazy4 Address.view plugins vc gc address
            )
                :: svg
        )
        []
        >> Keyed.node "g" []

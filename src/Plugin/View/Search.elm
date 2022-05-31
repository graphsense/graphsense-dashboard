module Plugin.View.Search exposing (..)

import Config.View as View
import Dict
import Plugin as Plugin exposing (..)
import Plugin.Model as Plugin exposing (..)


placeholder : Plugins -> View.Config -> List String
placeholder plugins vc =
    plugins
        |> Dict.toList
        |> List.map
            (\( pid, plugin ) ->
                plugin.view.search.placeholder vc
            )
        |> List.concat

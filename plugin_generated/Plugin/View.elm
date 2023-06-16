module Plugin.View exposing (..)

import Config.View as View
import Html.Styled as Html exposing (Html)
import Model
import Model.Graph.Address exposing (Address)
import Util.View
import Msg.Graph as Graph
import Msg.Search as Search
import Plugin.Model
import Plugin.Msg
import PluginInterface.View
import Svg.Styled as Svg exposing (Svg)
import Tuple exposing (..)


type alias Plugins =
    { 
    }


addressFlags : Plugins -> Plugin.Model.AddressState -> View.Config -> (Float, List (Svg Graph.Msg))
addressFlags plugins addressState vc =
    [ 
    ]
        |> List.filterMap identity
        |> List.foldl
            (\( width, flags ) ( accWidth, accFlags ) ->
                ( accWidth + width, accFlags ++ flags )
            )
            ( 0, [] )
        |> mapSecond (List.map (Svg.map Graph.PluginMsg))


entityFlags : Plugins -> Plugin.Model.EntityState -> View.Config -> (Float, List (Svg Graph.Msg))
entityFlags plugins entityState vc =
    [ 
    ]
        |> List.filterMap identity
        |> List.foldl
            (\( width, flags ) ( accWidth, accFlags ) ->
                ( accWidth + width, accFlags ++ flags )
            )
            ( 0, [] )
        |> mapSecond (List.map (Svg.map Graph.PluginMsg))


addressContextMenu : Plugins -> Plugin.Model.ModelState -> View.Config -> Address -> List (Html Graph.Msg)
addressContextMenu plugins states vc address =
    [ 
    ]
        |> List.filterMap identity
        |> List.map ((++) (Util.View.contextMenuRule vc))
        |> List.concat
        |> List.map (Html.map Graph.PluginMsg)


addressProperties : Plugins -> Plugin.Model.ModelState -> Plugin.Model.AddressState -> View.Config -> List (Html Graph.Msg)
addressProperties plugins states addressStates vc =
    [ 
    ]
        |> List.filterMap identity
        |> List.concat
        |> List.map (Html.map Graph.PluginMsg)


entityProperties : Plugins -> Plugin.Model.ModelState -> Plugin.Model.EntityState -> View.Config -> List (Html Graph.Msg)
entityProperties plugins states entityStates vc =
    [ 
    ]
        |> List.filterMap identity
        |> List.concat
        |> List.map (Html.map Graph.PluginMsg)


browser : Plugins -> View.Config -> Plugin.Model.ModelState -> List (Html Graph.Msg)
browser plugins vc states =
    [ 
    ]
        |> List.filterMap identity
        |> List.concat
        |> List.map (Html.map Graph.PluginMsg)


graphNavbarLeft : Plugins -> Plugin.Model.ModelState -> View.Config -> List (Html Graph.Msg)
graphNavbarLeft plugins states vc =
    [ 
    ]
        |> List.filterMap identity
        |> List.concat
        |> List.map (Html.map Graph.PluginMsg)


searchPlaceholder : Plugins -> View.Config -> List String
searchPlaceholder plugins vc =
    [ 
    ]
        |> List.filterMap identity


searchResultList : Plugins -> Plugin.Model.ModelState -> View.Config -> List (Html Search.Msg)
searchResultList plugins states vc =
    [ 
    ]
        |> List.filterMap identity
        |> List.concat
        |> List.map (Html.map Search.PluginMsg)


sidebar : Plugins -> Plugin.Model.ModelState -> Model.Page -> View.Config -> List (Html Model.Msg)
sidebar plugins states page vc =
    [ 
    ]
        |> List.filterMap identity
        |> List.concat
        |> List.map (Html.map Model.PluginMsg)


contents : Plugins -> Plugin.Model.ModelState -> Plugin.Model.PluginType -> View.Config -> Maybe (List (Html Model.Msg))
contents plugins states type_ vc =
    Nothing

navbar : Plugins -> Plugin.Model.ModelState -> Plugin.Model.PluginType -> View.Config -> Maybe (List (Html Model.Msg))
navbar plugins states type_ vc =
    Nothing

hovercards : Plugins -> Plugin.Model.ModelState -> View.Config -> List (Html Graph.Msg)
hovercards plugins states vc =
    [ 
    ]
        |> List.filterMap identity
        |> List.concat
        |> List.map (Html.map Graph.PluginMsg)


title : Plugins -> Plugin.Model.ModelState -> View.Config -> List String
title plugins states vc =
    [ 
    ]
        |> List.filterMap identity
        |> List.concat


profile : Plugins -> Plugin.Model.ModelState -> View.Config -> List (String, Html Model.Msg)
profile plugins states vc =
    [ 
    ]
        |> List.filterMap identity
        |> List.concat
        |> List.map (mapSecond (Html.map Model.PluginMsg))


login : Plugins -> Plugin.Model.ModelState -> View.Config -> List (Html Model.Msg)
login plugins states vc =
    [ 
    ]
        |> List.filterMap identity
        |> List.concat
        |> List.map (Html.map Model.PluginMsg)

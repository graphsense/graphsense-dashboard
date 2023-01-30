module PluginInterface.View exposing (..)

import Config.View as View
import Html.Styled exposing (Html)
import Model.Graph.Address exposing (Address)
import Svg.Styled exposing (Svg)


type alias View modelState addressState entityState msg =
    { -- address flags
      addressFlags : Maybe (View.Config -> addressState -> ( Float, List (Svg msg) ))

    -- entity flags
    , entityFlags : Maybe (View.Config -> entityState -> ( Float, List (Svg msg) ))

    -- address context menu
    , addressContextMenu : Maybe (View.Config -> Address -> modelState -> addressState -> List (Html msg))

    -- additional properties shown in the address's property box
    , addressProperties : Maybe (View.Config -> modelState -> addressState -> List (Html msg))

    -- additional properties shown in the entity's property box
    , entityProperties : Maybe (View.Config -> modelState -> entityState -> List (Html msg))

    -- browser contents
    , browser : Maybe (View.Config -> modelState -> List (Html msg))

    -- additional stuff for the left part of the graph's navbar
    , graphNavbarLeft : Maybe (View.Config -> modelState -> List (Html msg))

    -- additional strings for the search bar placeholder
    , searchPlaceholder : Maybe (View.Config -> String)

    -- additional results for the search bar result list
    , searchResultList : Maybe (View.Config -> modelState -> List (Html msg))

    -- additional stuff of the global sidebar
    , sidebar : Maybe (View.Config -> Bool -> modelState -> List (Html msg))

    -- navbar of the main pane
    , navbar : Maybe (View.Config -> modelState -> List (Html msg))

    -- contents of the main pane
    , contents : Maybe (View.Config -> modelState -> List (Html msg))

    -- show hovercards
    , hovercards : Maybe (View.Config -> modelState -> List (Html msg))

    -- update window's title
    , title : Maybe (View.Config -> modelState -> List String)

    -- additional stuff for the user's profile
    , profile : Maybe (View.Config -> modelState -> List ( String, Html msg ))
    }


init : View modelState addressState entityState msg
init =
    { addressFlags = Nothing
    , entityFlags = Nothing
    , addressContextMenu = Nothing
    , addressProperties = Nothing
    , entityProperties = Nothing
    , browser = Nothing
    , graphNavbarLeft = Nothing
    , searchPlaceholder = Nothing
    , searchResultList = Nothing
    , sidebar = Nothing
    , navbar = Nothing
    , contents = Nothing
    , hovercards = Nothing
    , title = Nothing
    , profile = Nothing
    }

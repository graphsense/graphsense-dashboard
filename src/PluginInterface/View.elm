module PluginInterface.View exposing (View, init)

import Config.View as View
import Html.Styled exposing (Html)
import Model.Pathfinder.Address as Pathfinder
import Svg.Styled exposing (Svg)
import View.Pathfinder.ContextMenuItem exposing (ContextMenuItem)


type alias View modelState addressState msg =
    { -- address context menu item for new pathfinder
      addressContextMenuNew : Maybe (View.Config -> Pathfinder.Address -> modelState -> addressState -> List (ContextMenuItem msg))

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

    -- update window's title
    , title : Maybe (View.Config -> modelState -> List String)

    -- additional stuff for the user's profile
    , profile : Maybe (View.Config -> modelState -> List ( String, Html msg ))

    -- additional stuff for login
    , login : Maybe (View.Config -> modelState -> List (Html msg))

    -- additional stuff for the new Pathfinder's address side panel header
    , addressSidePanelHeader : Maybe (View.Config -> Pathfinder.Address -> modelState -> addressState -> Html msg)
    , addressSidePanelHeaderWithPriority : Maybe (View.Config -> Pathfinder.Address -> modelState -> addressState -> Maybe { priority : Int, content : Html msg })

    -- additional stuff for the new Pathfinder's address side panel header tags
    , addressSidePanelHeaderTags : Maybe (View.Config -> Pathfinder.Address -> modelState -> addressState -> Maybe (Html msg))

    -- show a dialog
    , dialog : Maybe (View.Config -> modelState -> Maybe (Html msg))

    -- show a tooltip
    , tooltip : Maybe (View.Config -> modelState -> Maybe (List (Html msg)))

    -- Upper left panel in pathfinder (right besides the logo)
    , pathfinderUpperLeftPanel : Maybe (View.Config -> modelState -> Html msg)

    -- allows to add a tag icon on the address node
    , addressNodeTagIcon : Maybe (View.Config -> Pathfinder.Address -> addressState -> Maybe (Svg msg))

    -- allows to add legend items from the plugins
    , getLegendIconItems : Maybe (View.Config -> List { description : String, icon : Html msg, label : String })
    }


init : View modelState addressState msg
init =
    { addressContextMenuNew = Nothing
    , searchPlaceholder = Nothing
    , searchResultList = Nothing
    , sidebar = Nothing
    , navbar = Nothing
    , contents = Nothing
    , title = Nothing
    , profile = Nothing
    , login = Nothing
    , addressSidePanelHeader = Nothing
    , addressSidePanelHeaderWithPriority = Nothing
    , addressSidePanelHeaderTags = Nothing
    , dialog = Nothing
    , tooltip = Nothing
    , pathfinderUpperLeftPanel = Nothing
    , addressNodeTagIcon = Nothing
    , getLegendIconItems = Nothing
    }

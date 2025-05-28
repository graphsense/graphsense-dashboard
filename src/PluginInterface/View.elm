module PluginInterface.View exposing (View, init)

import Config.Graph as Graph
import Config.View as View
import Html.Styled exposing (Html)
import Model.Address as A
import Model.Entity as E
import Model.Graph.Address as Graph
import Model.Node as Node
import Model.Pathfinder.Address as Pathfinder
import Svg.Styled exposing (Svg)
import View.Pathfinder.ContextMenuItem exposing (ContextMenuItem)


type alias View modelState addressState entityState msg =
    { -- address flags
      addressFlags : Maybe (View.Config -> addressState -> ( Float, List (Svg msg) ))

    -- entity flags
    , entityFlags : Maybe (View.Config -> entityState -> ( Float, List (Svg msg) ))

    -- address context menu
    , addressContextMenu : Maybe (View.Config -> Graph.Address -> modelState -> addressState -> List (Html msg))

    -- address context menu item for new pathfinder
    , addressContextMenuNew : Maybe (View.Config -> Pathfinder.Address -> modelState -> addressState -> List (ContextMenuItem msg))

    -- additional properties shown in the address's property box
    , addressProperties : Maybe (View.Config -> Graph.Config -> modelState -> addressState -> List (Html msg))

    -- additional properties shown in the entity's property box
    , entityProperties : Maybe (View.Config -> Graph.Config -> modelState -> entityState -> List (Html msg))

    -- browser contents
    -- functor for checking whether a node is visible in the graph
    , browser : Maybe (View.Config -> Graph.Config -> (Node.Node A.Address E.Entity -> Bool) -> modelState -> List (Html msg))

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
    , tooltip : Maybe (View.Config -> { context : String, domId : String } -> modelState -> Maybe (List (Html msg)))

    -- Upper left panel in pathfinder (right besides the logo)
    , pathfinderUpperLeftPanel : Maybe (View.Config -> modelState -> Html msg)

    -- allows to replace the tag icon on the address node with something more specific
    , replaceAddressNodeTagIcon : Maybe (View.Config -> { hasTags : Bool } -> Pathfinder.Address -> addressState -> Maybe (Svg msg))

    -- allows to add legend items from the plugins
    , getLegendIconItems : Maybe (View.Config -> List { description : String, icon : Html msg, label : String })
    }


init : View modelState addressState entityState msg
init =
    { addressFlags = Nothing
    , entityFlags = Nothing
    , addressContextMenu = Nothing
    , addressContextMenuNew = Nothing
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
    , login = Nothing
    , addressSidePanelHeader = Nothing
    , addressSidePanelHeaderWithPriority = Nothing
    , addressSidePanelHeaderTags = Nothing
    , dialog = Nothing
    , tooltip = Nothing
    , pathfinderUpperLeftPanel = Nothing
    , replaceAddressNodeTagIcon = Nothing
    , getLegendIconItems = Nothing
    }

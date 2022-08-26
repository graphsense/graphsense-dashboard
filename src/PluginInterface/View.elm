module PluginInterface.View exposing (..)

import Config.View as View
import Html.Styled exposing (Html)
import Model.Graph.Address exposing (Address)
import Svg.Styled exposing (Svg)


type alias View modelState addressState entityState msg =
    { addressFlags : Maybe (View.Config -> addressState -> ( Float, List (Svg msg) ))
    , entityFlags : Maybe (View.Config -> entityState -> ( Float, List (Svg msg) ))
    , addressContextMenu : Maybe (View.Config -> Address -> modelState -> addressState -> List (Html msg))
    , addressProperties : Maybe (View.Config -> modelState -> addressState -> List (Html msg))
    , entityProperties : Maybe (View.Config -> modelState -> entityState -> List (Html msg))
    , browser : Maybe (View.Config -> modelState -> List (Html msg))
    , navbarLeft : Maybe (View.Config -> modelState -> List (Html msg))
    , searchPlaceholder : Maybe (View.Config -> String)
    , searchResultList : Maybe (View.Config -> modelState -> List (Html msg))
    , sidebar : Maybe (View.Config -> Bool -> modelState -> List (Html msg))
    , main : Maybe (View.Config -> modelState -> Html msg)
    , hovercards : Maybe (View.Config -> modelState -> List (Html msg))
    }


init : View modelState addressState entityState msg
init =
    { addressFlags = Nothing
    , entityFlags = Nothing
    , addressContextMenu = Nothing
    , addressProperties = Nothing
    , entityProperties = Nothing
    , browser = Nothing
    , navbarLeft = Nothing
    , searchPlaceholder = Nothing
    , searchResultList = Nothing
    , sidebar = Nothing
    , main = Nothing
    , hovercards = Nothing
    }

module Theme.Theme exposing (..)

import Color
import Css exposing (Style)
import Theme.Autocomplete as Autocomplete exposing (Autocomplete)
import Theme.Browser as Browser exposing (Browser)
import Theme.Button as Button exposing (Button)
import Theme.ContextMenu as ContextMenu exposing (ContextMenu)
import Theme.Dialog as Dialog exposing (Dialog)
import Theme.Graph as Graph exposing (Graph)
import Theme.Hovercard as Hovercard exposing (Hovercard)
import Theme.Search as Search exposing (Search)
import Theme.Stats as Stats exposing (Stats)
import Theme.Statusbar as Statusbar exposing (Statusbar)
import Theme.Table as Table exposing (Table)
import Theme.User as User exposing (User)


type alias Theme =
    { scaled : Float -> Float
    , logo : String
    , logo_lightmode : String
    , body : Bool -> List Style
    , sectionBelowHeader : List Style
    , header : Bool -> List Style
    , headerLogo : List Style
    , headerLogoWrap : List Style
    , headerTitle : List Style
    , heading2 : List Style
    , inputRaw : Bool -> Maybe Float -> List ( String, String )
    , addonsNav : List Style
    , sidebar : Bool -> List Style
    , sidebarIcon : Bool -> Bool -> List Style
    , main : Bool -> List Style
    , link : Bool -> List Style
    , loadingSpinner : List Style
    , loadingSpinnerUrl : String
    , overlay : List Style
    , popup : Bool -> List Style
    , stats : Stats
    , search : Search
    , autocomplete : Autocomplete
    , button : Button
    , graph : Graph
    , browser : Browser
    , contextMenu : ContextMenu
    , table : Table
    , tool : List Style
    , dialog : Dialog
    , user : User
    , statusbar : Statusbar
    , footer : List Style
    , hovercard : Bool -> Hovercard
    , buttonsRow : List Style
    , custom : String
    , switchLabel : List Style
    , switchRoot : List Style
    , switchOnColor : Bool -> Color.Color
    }


default : Theme
default =
    { scaled = (*) 1
    , logo = ""
    , logo_lightmode = ""
    , body = \_ -> []
    , sectionBelowHeader = []
    , header = \_ -> []
    , heading2 = []
    , inputRaw = \_ _ -> []
    , headerLogo = []
    , headerLogoWrap = []
    , headerTitle = []
    , addonsNav = []
    , sidebar = \_ -> []
    , sidebarIcon = \_ _ -> []
    , main = \_ -> []
    , link = \_ -> []
    , loadingSpinner = []
    , loadingSpinnerUrl = ""
    , overlay = []
    , popup = \_ -> []
    , stats = Stats.default
    , search = Search.default
    , autocomplete = Autocomplete.default
    , button = Button.default
    , tool = []
    , hovercard = \_ -> Hovercard.default
    , dialog = Dialog.default
    , user = User.default
    , statusbar = Statusbar.default
    , footer = []
    , graph = Graph.default
    , browser = Browser.default
    , contextMenu = ContextMenu.default
    , table = Table.default
    , buttonsRow = []
    , custom = ""
    , switchLabel = []
    , switchRoot = []
    , switchOnColor = \_ -> Color.rgba 0 0 0 0
    }


type alias SwitchableColor =
    { dark : Color.Color
    , light : Color.Color
    }
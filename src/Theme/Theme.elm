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
import Theme.Landingpage as Landingpage exposing (Landingpage)
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
    , paragraph : List Style
    , listItem : List Style
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
    , sidebarIconBottom : Bool -> Bool -> List Style
    , sidebarRule : Bool -> List Style
    , main : Bool -> List Style
    , navbar : Bool -> List Style
    , contents : Bool -> List Style
    , link : Bool -> List Style
    , iconLink : Bool -> List Style
    , loadingSpinner : List Style
    , loadingSpinnerUrl : String
    , userDefautImgUrl : String
    , overlay : List Style
    , popup : Bool -> List Style
    , stats : Stats
    , search : Search
    , autocomplete : Autocomplete
    , button : Button
    , graph : Graph
    , landingpage : Landingpage
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
    , disabled : Bool -> List Style
    , copyIcon : Bool -> List Style
    , longIdentifier : List Style
    , hint : Bool -> List Style
    }


default : Theme
default =
    { scaled = (*) 1
    , logo = ""
    , logo_lightmode = ""
    , body = \_ -> []
    , paragraph = []
    , listItem = []
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
    , sidebarIconBottom = \_ _ -> []
    , sidebarRule = \_ -> []
    , main = \_ -> []
    , navbar = \_ -> []
    , contents = \_ -> []
    , link = \_ -> []
    , iconLink = \_ -> []
    , loadingSpinner = []
    , loadingSpinnerUrl = ""
    , userDefautImgUrl = ""
    , overlay = []
    , popup = \_ -> []
    , stats = Stats.default
    , search = Search.default
    , landingpage = Landingpage.default
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
    , disabled = \_ -> []
    , copyIcon = \_ -> []
    , longIdentifier = []
    , hint = \_ -> []
    }

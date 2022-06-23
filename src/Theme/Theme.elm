module Theme.Theme exposing (Theme, default)

import Css exposing (Style)
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
    , body : List Style
    , sectionBelowHeader : List Style
    , header : List Style
    , headerLogo : List Style
    , headerLogoWrap : List Style
    , headerTitle : List Style
    , heading2 : List Style
    , inputRaw : Maybe Float -> List ( String, String )
    , addonsNav : List Style
    , sidebar : List Style
    , sidebarIcon : Bool -> List Style
    , main : List Style
    , link : List Style
    , loadingSpinner : List Style
    , loadingSpinnerUrl : String
    , overlay : List Style
    , popup : List Style
    , stats : Stats
    , search : Search
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
    , hovercard : Hovercard
    , custom : String
    }


default : Theme
default =
    { scaled = (*) 1
    , logo = ""
    , body = []
    , sectionBelowHeader = []
    , header = []
    , heading2 = []
    , inputRaw = \_ -> []
    , headerLogo = []
    , headerLogoWrap = []
    , headerTitle = []
    , addonsNav = []
    , sidebar = []
    , sidebarIcon = \_ -> []
    , main = []
    , link = []
    , loadingSpinner = []
    , loadingSpinnerUrl = ""
    , overlay = []
    , popup = []
    , stats = Stats.default
    , search = Search.default
    , button = Button.default
    , tool = []
    , hovercard = Hovercard.default
    , dialog = Dialog.default
    , user = User.default
    , statusbar = Statusbar.default
    , footer = []
    , graph = Graph.default
    , browser = Browser.default
    , contextMenu = ContextMenu.default
    , table = Table.default
    , custom = ""
    }

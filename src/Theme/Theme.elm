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
import Theme.Table as Table exposing (Table)
import Theme.User as User exposing (User)


type alias Theme =
    { scaled : Float -> Float
    , logo : String
    , body : List Style
    , sectionBelowHeader : List Style
    , header : List Style
    , headerLogo : List Style
    , heading2 : List Style
    , input : List Style
    , addonsNav : List Style
    , main : List Style
    , link : List Style
    , loadingSpinnerUrl : String
    , stats : Stats
    , search : Search
    , button : Button
    , graph : Graph
    , browser : Browser
    , contextMenu : ContextMenu
    , table : Table
    , tool : List Style
    , modal : Dialog
    , user : User
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
    , input = []
    , headerLogo = []
    , addonsNav = []
    , main = []
    , link = []
    , loadingSpinnerUrl = ""
    , stats = Stats.default
    , search = Search.default
    , button = Button.default
    , tool = []
    , hovercard = Hovercard.default
    , modal = Dialog.default
    , user = User.default
    , graph = Graph.default
    , browser = Browser.default
    , contextMenu = ContextMenu.default
    , table = Table.default
    , custom = ""
    }

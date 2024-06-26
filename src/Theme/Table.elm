module Theme.Table exposing (Table, default)

import Css exposing (Style)


type alias Table =
    { root : List Style
    , tableRoot : List Style
    , sidebar : Bool -> List Style
    , sidebarIcon : Bool -> Bool -> List Style
    , filter : List Style
    , filterInput : Bool -> List Style
    , table : List Style
    , headCell : Bool -> List Style
    , headRow : List Style
    , row : Bool -> List Style
    , maxHeight : Float
    , rowHeight : Float
    , headCellSortable : List Style
    , cell : List Style
    , numberCell : List Style
    , valuesCell : Bool -> Bool -> List Style
    , loadingSpinner : List Style
    , urlMaxLength : Int
    , emptyHint : List Style
    , tick : List Style
    , info : Bool -> List Style
    }


default : Table
default =
    { root = []
    , tableRoot = []
    , sidebar = \_ -> []
    , sidebarIcon = \_ _ -> []
    , filter = []
    , filterInput = \_ -> []
    , table = []
    , row = \_ -> []
    , headCell = \_ -> []
    , headRow = []
    , headCellSortable = []
    , maxHeight = 250
    , rowHeight = 15
    , cell = []
    , numberCell = []
    , valuesCell = \_ _ -> []
    , loadingSpinner = []
    , urlMaxLength = 40
    , emptyHint = []
    , tick = []
    , info = \_ -> []
    }

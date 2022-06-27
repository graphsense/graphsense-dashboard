module Theme.Table exposing (Table, default)

import Color exposing (Color)
import Css exposing (Style)


type alias Table =
    { root : List Style
    , tableRoot : List Style
    , sidebar : List Style
    , filter : List Style
    , filterInput : List Style
    , table : List Style
    , headCell : List Style
    , headRow : List Style
    , row : List Style
    , maxHeight : Float
    , rowHeight : Float
    , headCellSortable : List ( String, String )
    , cell : List Style
    , numberCell : List Style
    , valuesCell : Bool -> List Style
    , loadingSpinner : List Style
    , urlMaxLength : Int
    , emptyHint : List Style
    }


default : Table
default =
    { root = []
    , tableRoot = []
    , sidebar = []
    , filter = []
    , filterInput = []
    , table = []
    , row = []
    , headCell = []
    , headRow = []
    , headCellSortable = []
    , maxHeight = 250
    , rowHeight = 15
    , cell = []
    , numberCell = []
    , valuesCell = \_ -> []
    , loadingSpinner = []
    , urlMaxLength = 40
    , emptyHint = []
    }

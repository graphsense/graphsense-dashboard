module Theme.Table exposing (Table, default)

import Color exposing (Color)
import Css exposing (Style)


type alias Table =
    { root : List Style
    , table : List Style
    , headRow : List Style
    , row : List Style
    , headCellSortable : List ( String, String )
    , cell : List Style
    , valuesCell : Bool -> List Style
    }


default : Table
default =
    { root = []
    , table = []
    , row = []
    , headRow = []
    , headCellSortable = []
    , cell = []
    , valuesCell = \_ -> []
    }

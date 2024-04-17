module Theme.Pathfinder exposing (Pathfinder, default)

import Css exposing (Style)


type alias Pathfinder =
    { root : List Style
    , address : List Style
    , addressHandle : List Style
    , addressLabel : List Style
    , addressRadius : Float
    , addressSpacingToLabel : Float
    , tx : List Style
    , txRadius : Float
    , edgeCurvedEnd : Float
    , edgeLabelPadding : Float
    , edgeLabel : List Style
    , edgeUtxo : List Style
    , arrowLength : Float
    }


default : Pathfinder
default =
    { root = []
    , address = []
    , addressHandle = []
    , addressLabel = []
    , addressRadius = 10
    , addressSpacingToLabel = 5
    , tx = []
    , txRadius = 3
    , edgeCurvedEnd = 0.25
    , edgeLabelPadding = 5
    , edgeLabel = []
    , edgeUtxo = []
    , arrowLength = 5
    }

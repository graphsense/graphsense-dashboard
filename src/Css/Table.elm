module Css.Table exposing (Styles, row, styles, table)

import Config.View as View exposing (Config)
import Css exposing (..)
import Theme.Colors as Colors


root : Config -> List Style
root vc =
    [ displayFlex
    , flexDirection Css.row
    , overflowX auto
    , position relative
    ]


tableRoot : Config -> List Style
tableRoot vc =
    [ overflowY auto
    , overflowX auto
    , displayFlex
    , flexDirection column
    ]


table : Config -> List Style
table vc =
    [ 0.22 |> rem |> padding
    ]


fontBold : Style
fontBold =
    fontWeight (int 500)


headCell : Config -> List Style
headCell _ =
    [ textAlign left
    , fontBold
    , position sticky
    , top <| px 0
    , zIndex <| int 1
    , verticalAlign middle
    ]


headRow : Config -> List Style
headRow _ =
    [ textAlign left
    , fontBold
    , position sticky
    , top <| px 0
    , zIndex <| int 1
    ]


headCellSortable : Config -> List Style
headCellSortable vc =
    [ cursor pointer
    ]


row : Config -> List Style
row vc =
    [ nthChild "2n"
        [ Css.property "background-color" Colors.greyBlue20
        ]
    , nthChild "2n+1"
        [ Css.property "background-color" Colors.white
        ]
    , 20 |> px |> height
    ]


cell : Config -> List Style
cell vc =
    [ tableCell ]


valuesCell : Config -> Bool -> List Style
valuesCell vc isNegative =
    numberCell vc
        ++ (if isNegative then
                [ Css.property "color" Colors.red100 ]

            else
                []
           )


tableCell : Style
tableCell =
    [ 0.22 |> rem |> padding
    , whiteSpace noWrap
    , verticalAlign middle
    ]
        |> batch


numberCell : Config -> List Style
numberCell _ =
    [ tableCell
    , textAlign right
    ]


loadingSpinner : Config -> List Style
loadingSpinner vc =
    [ px 24 |> height
    , px 24 |> width
    , px 12 |> padding
    ]


emptyHint : Config -> List Style
emptyHint vc =
    [ displayFlex
    , flexGrow (int 1)
    , alignItems center
    , justifyContent center
    ]


tick : Config -> List Style
tick vc =
    [ display inlineBlock ]


info : Config -> List Style
info vc =
    [ position absolute
    , bottom zero
    , left zero
    ]


type alias Styles =
    { root : View.Config -> List Style
    , tableRoot : View.Config -> List Style
    , loadingSpinner : View.Config -> List Style
    , table : View.Config -> List Style
    , row : View.Config -> List Style
    , headRow : View.Config -> List Style
    , cell : View.Config -> List Style
    , headCellSortable : View.Config -> List Style
    , headCell : View.Config -> List Style
    , numberCell : View.Config -> List Style
    , valuesCell : View.Config -> Bool -> List Style
    , tick : View.Config -> List Style
    , info : View.Config -> List Style
    , emptyHint : View.Config -> List Style
    }


styles : Styles
styles =
    { root = root
    , tableRoot = tableRoot
    , loadingSpinner = loadingSpinner
    , table = table
    , row = row
    , headRow = headRow
    , cell = cell
    , headCellSortable = headCellSortable
    , headCell = headCell
    , numberCell = numberCell
    , valuesCell = valuesCell
    , tick = tick
    , info = info
    , emptyHint = emptyHint
    }

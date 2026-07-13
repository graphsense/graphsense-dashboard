module TableSorterTest exposing (suite)

import Expect
import Table
import Test exposing (Test, describe, test)


type alias Row =
    { id : String
    , value : Int
    }


{-| "a", "b" and "d" tie on value.
-}
rows : List Row
rows =
    [ Row "a" 5
    , Row "b" 5
    , Row "c" 3
    , Row "d" 5
    ]


sortDescending : List Row -> List Row
sortDescending =
    Table.applySorter False (Table.decreasingOrIncreasingBy .value)


suite : Test
suite =
    describe "Table.applySorter"
        [ test "descending sort keeps tied rows in their original order (stable)" <|
            \_ ->
                sortDescending rows
                    |> List.map .id
                    |> Expect.equal [ "a", "b", "d", "c" ]
        , test "reversed increasingOrDecreasingBy keeps tied rows in their original order" <|
            \_ ->
                Table.applySorter True (Table.increasingOrDecreasingBy .value) rows
                    |> List.map .id
                    |> Expect.equal [ "a", "b", "d", "c" ]
        , test "re-sorting sorted data does not toggle tied rows (idempotent)" <|
            \_ ->
                sortDescending (sortDescending rows)
                    |> Expect.equal (sortDescending rows)
        ]

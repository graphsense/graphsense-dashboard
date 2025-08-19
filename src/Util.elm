module Util exposing (allAndNotEmpty, and, n, removeLeading0x)


removeLeading0x : String -> String
removeLeading0x s =
    if String.startsWith "0x" s then
        s |> String.dropLeft 2

    else
        s


n : m -> ( m, List eff )
n m =
    ( m, [] )


and : (m -> ( m, List eff )) -> ( m, List eff ) -> ( m, List eff )
and update ( m, eff ) =
    let
        ( m2, eff2 ) =
            update m
    in
    ( m2
    , eff ++ eff2
    )


allAndNotEmpty : (a -> Bool) -> List a -> Bool
allAndNotEmpty pred list =
    if List.isEmpty list then
        False

    else
        List.all pred list

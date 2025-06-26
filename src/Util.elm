module Util exposing (allAndNotEmpty, and, n)


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

module Util exposing (and, n)


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

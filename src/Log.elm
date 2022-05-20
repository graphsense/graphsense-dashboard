module Log exposing (..)


log : String -> a -> a
log str a =
    --a
    Debug.log str a


truncate : String -> a -> a
truncate str a =
    let
        _ =
            Debug.toString a
                |> String.left 100
                |> Debug.log str
    in
    a

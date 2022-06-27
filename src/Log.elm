module Log exposing (..)


log : String -> a -> a
log str a =
    a


log2 : String -> a -> a
log2 str a =
    --Debug.log str a
    a


truncate : String -> a -> a
truncate str a =
    {- let
           _ =
               Debug.toString a
                   |> String.left 100
                   |> Debug.log str
       in
    -}
    a

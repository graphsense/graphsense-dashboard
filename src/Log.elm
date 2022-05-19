module Log exposing (log)


log : String -> a -> a
log str a =
    Debug.log str a



--a

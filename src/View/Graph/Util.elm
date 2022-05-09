module View.Graph.Util exposing (..)


translate : Float -> Float -> String
translate x y =
    "translate(" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ")"


rotate : Float -> String -> String
rotate degree others =
    others ++ " rotate(" ++ String.fromFloat degree ++ ")"

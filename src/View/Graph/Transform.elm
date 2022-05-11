module View.Graph.Transform exposing (viewBox)

import Init.Graph.Transform exposing (..)
import Model.Graph.Transform exposing (..)
import Update.Graph.Transform exposing (..)


viewBox : { width : Float, height : Float } -> Model -> String
viewBox { width, height } model =
    let
        transform =
            get model
    in
    [ transform.x
    , transform.y
    , max 0 <| width * transform.z
    , max 0 <| height * transform.z
    ]
        |> List.map String.fromFloat
        |> List.intersperse " "
        |> String.concat

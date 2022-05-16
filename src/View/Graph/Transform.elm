module View.Graph.Transform exposing (viewBox)

import Config.Graph exposing (entityMinHeight, entityWidth, expandHandleWidth)
import Init.Graph.Transform exposing (..)
import Model.Graph.Transform exposing (..)
import Update.Graph.Transform exposing (..)


viewBox : { width : Float, height : Float } -> Model -> String
viewBox { width, height } model =
    [ model.x + entityWidth / 2 - expandHandleWidth - width / 2 * model.z
    , model.y + entityMinHeight - height / 2 * model.z
    , max 0 <| width * model.z
    , max 0 <| height * model.z
    ]
        |> List.map String.fromFloat
        |> List.intersperse " "
        |> String.concat

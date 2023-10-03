module View.Graph.Transform exposing (viewBox)

import Config.Graph exposing (addressHeight, entityMinHeight, entityWidth, expandHandleWidth)
import Model.Graph.Transform exposing (..)


viewBox : { width : Float, height : Float } -> Model -> String
viewBox { width, height } model =
    [ model.x + (2 * expandHandleWidth + entityWidth) / 2 - width / 2 * model.z
    , model.y + (entityMinHeight + addressHeight) / 2 - height / 2 * model.z
    , max 0 <| width * model.z
    , max 0 <| height * model.z
    ]
        |> List.map String.fromFloat
        |> List.intersperse " "
        |> String.concat

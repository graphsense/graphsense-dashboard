module Init.Pathfinder.Conversion exposing (init)

import Api.Data
import Model.Pathfinder.Conversion exposing (Conversion)
import Model.Pathfinder.Id exposing (Id)
import Tuple exposing (first, second)


init : Api.Data.ExternalConversion -> ( Id, Id ) -> ( String, String ) -> Conversion
init apiConversion edge assets =
    { inputId = first edge
    , outputId = second edge
    , fromAsset = assets |> first |> String.toUpper
    , toAsset = assets |> second |> String.toUpper
    , inputAddress = Nothing
    , outputAddress = Nothing
    , raw = apiConversion
    , selected = False
    , hovered = False
    }

module Init.Pathfinder.Conversion exposing (init)

import Api.Data
import Model.Pathfinder.Conversion exposing (Conversion)
import Model.Pathfinder.Id exposing (Id)
import Tuple exposing (first, second)


init : Api.Data.ExternalConversion -> ( Id, Id ) -> Conversion
init apiConversion edge =
    { inputId = first edge
    , outputId = second edge
    , inputAddress = Nothing
    , outputAddress = Nothing
    , raw = apiConversion
    , selected = False
    , hovered = False
    }

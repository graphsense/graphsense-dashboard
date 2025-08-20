module Model.Pathfinder.Conversion exposing (Conversion, toIdString)

import Api.Data
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id as Id exposing (Id)


type alias Conversion =
    { outputId : Id
    , inputId : Id
    , inputAddress : Maybe Address
    , outputAddress : Maybe Address
    , raw : Api.Data.ExternalConversion
    , selected : Bool
    , hovered : Bool
    }


toIdString : Conversion -> String
toIdString conversion =
    idToString ( conversion.outputId, conversion.inputId )


idToString : ( Id, Id ) -> String
idToString ( outputId, inputId ) =
    Id.toString outputId ++ "_" ++ Id.toString inputId

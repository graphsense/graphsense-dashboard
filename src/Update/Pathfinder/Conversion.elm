module Update.Pathfinder.Conversion exposing (setAddress, updateAddress)

import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Conversion exposing (Conversion)
import Model.Pathfinder.Id exposing (Id)


updateAddress : Id -> (Address -> Address) -> Conversion -> Conversion
updateAddress id upd conversion =
    if id == conversion.inputId then
        { conversion
            | inputAddress = Maybe.map upd conversion.inputAddress
        }

    else if id == conversion.outputId then
        { conversion
            | outputAddress = Maybe.map upd conversion.outputAddress
        }

    else
        conversion


setAddress : Maybe Address -> Conversion -> Conversion
setAddress ma conversion =
    ma
        |> Maybe.map
            (\a ->
                if a.id == conversion.inputId then
                    { conversion | inputAddress = Just a }

                else if a.id == conversion.outputId then
                    { conversion | outputAddress = Just a }

                else
                    conversion
            )
        |> Maybe.withDefault conversion

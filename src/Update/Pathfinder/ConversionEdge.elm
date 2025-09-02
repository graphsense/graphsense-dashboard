module Update.Pathfinder.ConversionEdge exposing (setAddress, updateAddress)

import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.ConversionEdge exposing (ConversionEdge)
import Model.Pathfinder.Id exposing (Id)


updateAddress : Id -> (Address -> Address) -> ConversionEdge -> ConversionEdge
updateAddress id upd conversion =
    let
        c1 =
            if id == conversion.inputAddressId then
                { conversion
                    | inputAddress = Maybe.map upd conversion.inputAddress
                }

            else
                conversion
    in
    if id == conversion.outputAddressId then
        { c1
            | outputAddress = Maybe.map upd conversion.outputAddress
        }

    else
        c1


setAddress : Maybe Address -> ConversionEdge -> ConversionEdge
setAddress ma conversion =
    ma
        |> Maybe.map
            (\a ->
                let
                    c1 =
                        if a.id == conversion.inputAddressId then
                            { conversion | inputAddress = Just a }

                        else
                            conversion
                in
                if a.id == conversion.outputAddressId then
                    { c1 | outputAddress = Just a }

                else
                    c1
            )
        |> Maybe.withDefault conversion

module Graph.Update.Adding exposing (addAddress, addLabel, checkAddress)

import Graph.Model.Adding exposing (Model)
import Set exposing (Set)


addAddress : { currency : String, address : String } -> Model -> Model
addAddress { currency, address } model =
    { model
        | addresses = Set.insert ( currency, address ) model.addresses
    }


addLabel : String -> Model -> Model
addLabel label model =
    { model
        | labels = Set.insert label model.labels
    }


checkAddress : { currency : String, address : String } -> Model -> Maybe Model
checkAddress { currency, address } model =
    if Set.member ( currency, address ) model.addresses then
        { model
            | addresses = Set.remove ( currency, address ) model.addresses
        }
            |> Just

    else
        Nothing

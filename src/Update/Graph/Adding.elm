module Update.Graph.Adding exposing (addAddress, addEntity, addLabel, checkAddress, checkEntity)

import Model.Graph.Adding exposing (Model)
import Set exposing (Set)


addAddress : { currency : String, address : String } -> Model -> Model
addAddress { currency, address } model =
    { model
        | addresses = Set.insert ( currency, address ) model.addresses
    }


addEntity : { currency : String, entity : Int } -> Model -> Model
addEntity { currency, entity } model =
    { model
        | entities = Set.insert ( currency, entity ) model.entities
    }


addLabel : String -> Model -> Model
addLabel label model =
    { model
        | labels = Set.insert label model.labels
    }


checkAddress : { currency : String, address : String } -> Model -> Model
checkAddress { currency, address } model =
    { model
        | addresses = Set.remove ( currency, address ) model.addresses
    }


checkEntity : { currency : String, entity : Int } -> Model -> Model
checkEntity { currency, entity } model =
    { model
        | entities = Set.remove ( currency, entity ) model.entities
    }

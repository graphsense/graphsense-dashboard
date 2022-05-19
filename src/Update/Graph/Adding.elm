module Update.Graph.Adding exposing (addEntity, addLabel, checkEntity, getAddress, loadAddress, removeAddress, setAddress)

import Api.Data
import Dict exposing (Dict)
import Model.Graph.Adding exposing (Model)
import RemoteData exposing (RemoteData(..))
import Set exposing (Set)


loadAddress : { currency : String, address : String } -> Model -> Model
loadAddress { currency, address } model =
    { model
        | addresses = Dict.insert ( currency, address ) Loading model.addresses
    }


setAddress : { currency : String, address : String } -> Api.Data.Address -> Model -> Model
setAddress { currency, address } a model =
    { model
        | addresses = Dict.insert ( currency, address ) (Success a) model.addresses
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


removeAddress : { currency : String, address : String } -> Model -> Model
removeAddress { currency, address } model =
    { model
        | addresses = Dict.remove ( currency, address ) model.addresses
    }


getAddress : { currency : String, address : String } -> Model -> Maybe Api.Data.Address
getAddress { currency, address } model =
    Dict.get ( currency, address ) model.addresses
        |> Maybe.andThen RemoteData.toMaybe


checkEntity : { currency : String, entity : Int } -> Model -> Model
checkEntity { currency, entity } model =
    { model
        | entities = Set.remove ( currency, entity ) model.entities
    }

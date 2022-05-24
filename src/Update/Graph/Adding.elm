module Update.Graph.Adding exposing (..)

import Api.Data
import Dict exposing (Dict)
import Init.Graph.Adding as Init
import Model.Graph.Adding exposing (..)
import RemoteData exposing (RemoteData(..))
import Set exposing (Set)


loadAddress : { currency : String, address : String } -> Model -> Model
loadAddress { currency, address } model =
    { model
        | addresses = Dict.insert ( currency, address ) Init.addresses model.addresses
    }


setAddress : { currency : String, address : String } -> Api.Data.Address -> Model -> Model
setAddress { currency, address } data model =
    { model
        | addresses =
            Dict.update
                ( currency, address )
                (Maybe.map (\a -> { a | address = Just data }))
                model.addresses
    }


setEntityForAddress : { currency : String, address : String } -> Api.Data.Entity -> Model -> Model
setEntityForAddress { currency, address } data model =
    { model
        | addresses =
            Dict.update
                ( currency, address )
                (Maybe.map (\a -> { a | entity = Just data }))
                model.addresses
    }


setOutgoingForAddress : { currency : String, address : String } -> List Api.Data.NeighborEntity -> Model -> Model
setOutgoingForAddress { currency, address } data model =
    { model
        | addresses =
            Dict.update
                ( currency, address )
                (Maybe.map (\a -> { a | outgoing = Just data }))
                model.addresses
    }


setIncomingForAddress : { currency : String, address : String } -> List Api.Data.NeighborEntity -> Model -> Model
setIncomingForAddress { currency, address } data model =
    { model
        | addresses =
            Dict.update
                ( currency, address )
                (Maybe.map (\a -> { a | incoming = Just data }))
                model.addresses
    }


loadEntity : { currency : String, entity : Int } -> Model -> Model
loadEntity { currency, entity } model =
    { model
        | entities = Dict.insert ( currency, entity ) Init.entities model.entities
    }


setEntityForEntity : { currency : String, entity : Int } -> Api.Data.Entity -> Model -> Model
setEntityForEntity { currency, entity } data model =
    { model
        | entities =
            Dict.update
                ( currency, entity )
                (Maybe.map (\a -> { a | entity = Just data }))
                model.entities
    }


setOutgoingForEntity : { currency : String, entity : Int } -> List Api.Data.NeighborEntity -> Model -> Model
setOutgoingForEntity { currency, entity } data model =
    { model
        | entities =
            Dict.update
                ( currency, entity )
                (Maybe.map (\a -> { a | outgoing = Just data }))
                model.entities
    }


setIncomingForEntity : { currency : String, entity : Int } -> List Api.Data.NeighborEntity -> Model -> Model
setIncomingForEntity { currency, entity } data model =
    { model
        | entities =
            Dict.update
                ( currency, entity )
                (Maybe.map (\a -> { a | incoming = Just data }))
                model.entities
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


readyAddress : { currency : String, address : String } -> Model -> Maybe { address : Api.Data.Address, entity : Api.Data.Entity, outgoing : List Api.Data.NeighborEntity, incoming : List Api.Data.NeighborEntity }
readyAddress { currency, address } model =
    Dict.get ( currency, address ) model.addresses
        |> Maybe.andThen
            (\add ->
                Maybe.map4
                    (\a e i o ->
                        { address = a
                        , entity = e
                        , incoming = i
                        , outgoing = o
                        }
                    )
                    add.address
                    add.entity
                    add.incoming
                    add.outgoing
            )


removeEntity : { currency : String, entity : Int } -> Model -> Model
removeEntity { currency, entity } model =
    { model
        | entities = Dict.remove ( currency, entity ) model.entities
    }


readyEntity : { currency : String, entity : Int } -> Model -> Maybe { entity : Api.Data.Entity, outgoing : List Api.Data.NeighborEntity, incoming : List Api.Data.NeighborEntity }
readyEntity { currency, entity } model =
    Dict.get ( currency, entity ) model.entities
        |> Maybe.andThen
            (\add ->
                Maybe.map3
                    (\e i o ->
                        { entity = e
                        , incoming = i
                        , outgoing = o
                        }
                    )
                    add.entity
                    add.incoming
                    add.outgoing
            )

module Graph.Init.Id exposing (..)

import Graph.Model.Id exposing (..)


initAddressId : { layer : Int, currency : String, id : String } -> AddressId
initAddressId i =
    ( i.layer, i.currency, i.id )


initEntityId : { layer : Int, currency : String, id : Int } -> EntityId
initEntityId i =
    ( i.layer, i.currency, i.id )

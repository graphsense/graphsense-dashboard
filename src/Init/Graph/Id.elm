module Init.Graph.Id exposing (initAddressId, initEntityId, initLinkId)

import Model.Graph.Id exposing (..)
import Tuple exposing (..)


initAddressId : { layer : Int, currency : String, id : String } -> AddressId
initAddressId i =
    ( i.layer, i.currency, i.id )


initEntityId : { layer : Int, currency : String, id : Int } -> EntityId
initEntityId i =
    ( i.layer, i.currency, i.id )


initLinkId : a -> a -> LinkId a
initLinkId =
    pair

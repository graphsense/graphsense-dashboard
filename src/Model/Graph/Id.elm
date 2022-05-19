module Model.Graph.Id exposing (..)

import Tuple exposing (..)


{-| A tuple of layer index, currency, address/entity id
-}
type alias AddressId =
    ( Int, String, String )


type alias EntityId =
    ( Int, String, Int )


type alias LinkId id =
    ( id, id )


layer : ( Int, currency, id ) -> Int
layer ( i, _, _ ) =
    i


currency : ( layer, String, id ) -> String
currency ( _, i, _ ) =
    i


addressId : AddressId -> String
addressId ( _, _, i ) =
    i


entityId : EntityId -> Int
entityId ( _, _, i ) =
    i


entityIdToString : EntityId -> String
entityIdToString ( l, c, i ) =
    String.fromInt l ++ c ++ String.fromInt i


addressIdToString : AddressId -> String
addressIdToString ( l, c, i ) =
    String.fromInt l ++ c ++ i


entityLinkIdToString : LinkId EntityId -> String
entityLinkIdToString ( s, t ) =
    entityIdToString s
        ++ entityIdToString t


addressLinkIdToString : LinkId AddressId -> String
addressLinkIdToString ( s, t ) =
    addressIdToString s
        ++ addressIdToString t


noEntityId : EntityId
noEntityId =
    ( 0, "", -1 )


noEntityLinkId : LinkId EntityId
noEntityLinkId =
    ( noEntityId, noEntityId )


noAddressId : AddressId
noAddressId =
    ( 0, "", "" )


noAddressLinkId : LinkId AddressId
noAddressLinkId =
    ( noAddressId, noAddressId )


getSourceId : LinkId a -> a
getSourceId =
    first
